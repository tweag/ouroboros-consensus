{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns        #-}

module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Proxy (Proxy (..))
import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Tracer

import qualified Network.Socket as Socket

import Options.Applicative

import qualified Codec.Serialise as CBOR

import Network.TypedProtocol.Codec

import qualified Network.Mux as Mx

import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mock.ConcreteBlock
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, nodeToNodeHandshakeCodec)

import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync

import DBServer.Parsers (parseAddr)
import DBServer.Types (HostAddr)

data Options = Options
  { addr :: HostAddr
  , port :: Socket.PortNumber
  , maxSlotNo :: Maybe SlotNo
  }
  deriving Show

optionsParser :: ParserInfo Options
optionsParser = 
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Run a client of the ImmutableDB server"

  parse = do
    addr <-
      option (eitherReader parseAddr) $
        mconcat
          [ long "addr"
          , help "Address of the server"
          , value (127, 0, 0, 1)
          , showDefault
          ]
    port <-
      option auto $
        mconcat
          [ long "port"
          , help "Port of the server"
          , value 3001
          , showDefault
          ]
    maxSlotNo <-
      option (Just . fromIntegral @Int <$> auto) $
        mconcat
          [ long "max-slot-no"
          , help "slot number after which toterminate"
          , value Nothing
          , metavar "SLOTNO"
          ]
    pure Options{addr, port, maxSlotNo}

main :: IO ()
main = do
    opts@Options{addr, port} <- execParser optionsParser
    print opts
    let sockAddr = Socket.SockAddrInet port (Socket.tupleToHostAddress addr)
    clientChainSync sockAddr (maxSlotNo opts)


-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = maxBound
    }


--
-- Chain sync demo
--

demoProtocol2
  :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b -- ^ chainSync
  -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocol2 chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = chainSync
      }
    ]


clientChainSync :: Socket.SockAddr
                -> Maybe SlotNo
                -> IO ()
clientChainSync sockAddr maxSlotNo = withIOManager $ \iocp ->
    do
      void $ connectToNode
        (socketSnocket iocp)
        makeSocketBearer
        ConnectToArgs {
          ctaHandshakeCodec      = nodeToNodeHandshakeCodec,
          ctaHandshakeTimeLimits = noTimeLimitsHandshake,
          ctaVersionDataCodec    = cborTermVersionDataCodec nodeToNodeCodecCBORTerm,
          ctaConnectTracers      = nullNetworkConnectTracers,
          ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
        }
        mempty
        (simpleSingletonVersions
           UnversionedProtocol
           UnversionedProtocolData
           (\_ -> app))
        Nothing
        sockAddr

  where
    app :: OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    app = demoProtocol2 $
          InitiatorProtocolOnly $
          mkMiniProtocolCbFromPeer $ \_ctx ->
            ( contramap show stdoutTracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer (chainSyncClient (continueForever Proxy) maxSlotNo)
            )


codecChainSync :: ( CBOR.Serialise block
                  , CBOR.Serialise point
                  , CBOR.Serialise tip
                  )
               => Codec (ChainSync.ChainSync block point tip)
                        CBOR.DeserialiseFailure
                        IO LBS.ByteString
codecChainSync =
    ChainSync.codecChainSync
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode
      CBOR.encode CBOR.decode


--
-- Chain sync and block fetch protocol handlers
--

chainSyncClient :: ControlMessageSTM IO
                -> Maybe SlotNo
                -> ChainSync.ChainSyncClient
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient controlMessageSTM maxSlotNo =
    ChainSync.ChainSyncClient $ do
      curvar   <- newTVarIO genesisAnchoredFragment
      chainvar <- newTVarIO genesisAnchoredFragment
      case chainSyncClient' controlMessageSTM maxSlotNo  nullTracer curvar chainvar of
        ChainSync.ChainSyncClient k -> k

chainSyncClient' :: ControlMessageSTM IO
                 -> Maybe SlotNo
                 -> Tracer IO (Point BlockHeader, Point BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> StrictTVar IO (AF.AnchoredFragment BlockHeader)
                 -> ChainSync.ChainSyncClient
                      BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
chainSyncClient' controlMessageSTM _maxSlotNo syncTracer _currentChainVar candidateChainVar =
    ChainSync.ChainSyncClient (return requestNext)
  where
    requestNext :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    requestNext =
      ChainSync.SendMsgRequestNext
        (pure ())   -- on MsgAwaitReply; could trace
        handleNext

    terminate :: ChainSync.ClientStIdle
                     BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    terminate = ChainSync.SendMsgDone ()

    handleNext :: ChainSync.ClientStNext
                    BlockHeader (Point BlockHeader) (Point BlockHeader) IO ()
    handleNext =
      ChainSync.ClientStNext {
        ChainSync.recvMsgRollForward  = \blockHeader _pHead ->
          ChainSync.ChainSyncClient $ do
            addBlock blockHeader
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext

      , ChainSync.recvMsgRollBackward = \pIntersect _pHead ->
          ChainSync.ChainSyncClient $ do
            rollback pIntersect
            cm <- atomically controlMessageSTM
            return $ case cm of
              Terminate -> terminate
              _         -> requestNext
      }

    addBlock :: BlockHeader -> IO ()
    addBlock b = do
        chain <- atomically $ do
          chain <- readTVar candidateChainVar
          let !chain' = shiftAnchoredFragment 50 b chain
          writeTVar candidateChainVar chain'
          return chain'
        traceWith syncTracer (AF.lastPoint chain, AF.headPoint chain)

    rollback :: Point BlockHeader -> IO ()
    rollback p = atomically $ do
        chain <- readTVar candidateChainVar
        -- we do not handle rollback failure in this demo
        let (Just !chain') = AF.rollback p chain
        writeTVar candidateChainVar chain'
    {-
    notTooFarAhead = atomically $ do
        currentChain   <- readTVar currentChainVar
        candidateChain <- readTVar candidateChainVar
        check $ case (AF.headBlockNo currentChain,
                      AF.headBlockNo candidateChain) of
                  (Just bn, Just bn') -> bn' < bn + 5
                  _                   -> True
    -}

--
-- Utils
--

genesisAnchoredFragment :: AF.AnchoredFragment BlockHeader
genesisAnchoredFragment = AF.Empty AF.AnchorGenesis

shiftAnchoredFragment :: HasHeader block
                      => Int
                      -> block
                      -> AF.AnchoredFragment block
                      -> AF.AnchoredFragment block
shiftAnchoredFragment n b af =
  AF.anchorNewest (fromIntegral (AF.length af - n)) af AF.:> b
