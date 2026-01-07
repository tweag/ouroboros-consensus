{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
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

import qualified Network.Mux as Mx

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import Ouroboros.Network.Util.ShowProxy (ShowProxy)

import DBServer.Parsers (parseAddr)
import DBServer.Tracing (getTrivialSendRecvTracer)
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
    -- getCodec :: NodeToNodeVersion -> BlockNodeToNodeVersion blk -> Codec (ChainSync.ChainSync () (Point blk) (Tip blk)) CBOR.DeserialiseFailure IO LBS.ByteString
    getCodec version blockVersion = Consensus.N2N.cChainSyncCodecSerialised $
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
            ( getTrivialSendRecvTracer stdoutTracer
            , getCodec version blockVersion
            , ChainSync.chainSyncClientPeer (chainSyncClient (continueForever Proxy) maxSlotNo)
            )


type H blk = SerialisedHeader blk

--
-- Chain sync protocol handlers
--

chainSyncClient :: 
  forall blk. 
  ( HasHeader blk
  ) => 
  ControlMessageSTM IO
  -> Maybe SlotNo
  -> ChainSync.ChainSyncClient (H blk) (Point blk) (Tip blk) IO ()
chainSyncClient controlMessageSTM maxSlotNo =
    ChainSync.ChainSyncClient $ do
      curvar   <- newTVarIO genesisAnchoredFragment
      chainvar <- newTVarIO genesisAnchoredFragment
      case chainSyncClient' controlMessageSTM maxSlotNo nullTracer curvar chainvar of
        ChainSync.ChainSyncClient k -> k

chainSyncClient' :: forall blk. (HasHeader blk) => ControlMessageSTM IO
                 -> Maybe SlotNo
                 -> Tracer IO (Point blk, Point blk)
                 -> StrictTVar IO (AF.AnchoredFragment blk)
                 -> StrictTVar IO (AF.AnchoredFragment blk)
                 -> ChainSync.ChainSyncClient (H blk) (Point blk) (Tip blk) IO ()
chainSyncClient' controlMessageSTM _maxSlotNo _ _currentChainVar candidateChainVar =
    ChainSync.ChainSyncClient (return requestNext)
  where
    requestNext :: ChainSync.ClientStIdle
                     (H blk) (Point blk) (Tip blk) IO ()
    requestNext =
      ChainSync.SendMsgRequestNext
        (pure ())   -- on MsgAwaitReply; could trace
        handleNext

    terminate :: ChainSync.ClientStIdle
                    (H blk) (Point blk) (Tip blk) IO ()
    terminate = ChainSync.SendMsgDone ()

    handleNext :: ChainSync.ClientStNext
                    (H blk) (Point blk) (Tip blk) IO ()
    handleNext =
      ChainSync.ClientStNext {
        ChainSync.recvMsgRollForward  = \h _pHead ->
          ChainSync.ChainSyncClient $ do
            addBlock h
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

    addBlock :: H blk -> IO ()
    addBlock _ = pure ()

    rollback :: Point blk -> IO ()
    rollback p = atomically $ do
        chain <- readTVar candidateChainVar
        -- we do not handle rollback failure in this demo
        let (Just !chain') = AF.rollback p chain
        writeTVar candidateChainVar chain'

genesisAnchoredFragment :: forall blk. (HasHeader blk) => AF.AnchoredFragment blk
genesisAnchoredFragment = AF.Empty AF.AnchorGenesis
