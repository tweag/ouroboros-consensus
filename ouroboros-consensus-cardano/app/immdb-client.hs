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
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Void (Void)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Tracer

import qualified Network.Socket as Socket

import Options.Applicative

import qualified Codec.Serialise as CBOR

import Network.TypedProtocol.Codec

import qualified Network.Mux as Mx

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mock.ConcreteBlock
import Ouroboros.Network.Mux
import Ouroboros.Network.Mux
  ( MiniProtocolCb (..)
  , OuroborosApplicationWithMinimalCtx
  )
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, nodeToNodeHandshakeCodec)
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Util.ShowProxy (ShowProxy)

import DBServer.Parsers (parseAddr)
import DBServer.Types (HostAddr)
import Ouroboros.Network.PeerSelection.PeerSharing.Codec (decodeRemoteAddress, encodeRemoteAddress)
import Ouroboros.Consensus.Node (stdVersionDataNTN)
import Ouroboros.Consensus.Node.NetworkProtocolVersion (BlockNodeToNodeVersion, SupportedNetworkProtocolVersion(supportedNodeToNodeVersions))
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode(getNetworkMagic))
import Ouroboros.Consensus.Config (configBlock, configCodec)

data Options = Options
  { addr :: HostAddr
  , port :: Socket.PortNumber
  , configFile :: FilePath
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
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    maxSlotNo <-
      option (Just . fromIntegral @Int <$> auto) $
        mconcat
          [ long "max-slot-no"
          , help "slot number after which toterminate"
          , value Nothing
          , metavar "SLOTNO"
          ]
    pure Options{addr, port, configFile, maxSlotNo}

main :: IO ()
main = do
    cryptoInit
    opts@Options{addr, port, configFile, maxSlotNo} <- execParser optionsParser
    print opts
    let sockAddr = Socket.SockAddrInet port (Socket.tupleToHostAddress addr)
        args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
    let cfgCodec = configCodec pInfoConfig
        networkMagic = getNetworkMagic . configBlock $ pInfoConfig
    clientChainSync sockAddr cfgCodec networkMagic maxSlotNo


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
        miniProtocolLimits = limits,
        miniProtocolRun    = chainSync
      }
    ]
  where limits = chainSyncProtocolLimits defaultMiniProtocolParameters


clientChainSync :: 
  forall blk.
  ( HasHeader blk
  , ShowProxy blk
  , SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  ) => 
  Socket.SockAddr
  -> CodecConfig blk
  -> NetworkMagic
  -> Maybe SlotNo
  -> IO ()
clientChainSync sockAddr codecCfg networkMagic maxSlotNo = withIOManager $ \iocp ->
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
        versions
        Nothing
        sockAddr

  where
    versions = Versions $ Map.mapWithKey mkVersion $ supportedNodeToNodeVersions (Proxy @blk)
     where
      mkVersion version blockVersion =
        Version
          { versionApplication = const $ mkApp version blockVersion
          , versionData =
              stdVersionDataNTN
                networkMagic
                InitiatorOnlyDiffusionMode
                PeerSharingDisabled
          }
    -- versions = (simpleSingletonVersions
    --        UnversionedProtocol
    --        UnversionedProtocolData
    --        (\_ -> app))
    getCodec codecCfg blockVersion version = Consensus.N2N.cChainSyncCodecSerialised $
      Consensus.N2N.defaultCodecs
        codecCfg 
        blockVersion
        encodeRemoteAddress 
        decodeRemoteAddress 
        version
    mkApp :: 
      NodeToNodeVersion ->
      BlockNodeToNodeVersion blk ->
      OuroborosApplicationWithMinimalCtx Mx.InitiatorMode addr LBS.ByteString IO () Void
    mkApp version blockVersion = demoProtocol2 $
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
