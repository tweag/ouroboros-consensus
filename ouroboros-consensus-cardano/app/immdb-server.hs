{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Node.Tracing (startResourceTracer)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import qualified Cardano.Tools.ImmDBServer.RemoteStorage as RemoteStorage
import Data.Void
import Main.Utf8 (withStdTerminalHandles)
import qualified Network.Socket as Socket
import Options.Applicative
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Ouroboros.Consensus.Util.IPOctets (IPOctets, fromIPTuple, parseIPOctets, printIP, toIPTuple)
import "contra-tracer" Control.Tracer (showTracing, stdoutTracer, traceWith)

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  Opts{immDBDir, ip, port, configFile, rtsFrequency, remoteStorageCacheDir, remoteStorageSrcUrl, maxCachedChunks} <-
    execParser optsParser
  let sockAddr = Socket.SockAddrInet port (Socket.tupleToHostAddress (toIPTuple ip))
      args = Cardano.CardanoBlockArgs configFile Nothing
      eventTracer = showTracing stdoutTracer
      -- msgTracer = getTrivialSendRecvTracer stdoutTracer
      msgTracer = showTracing stdoutTracer
  ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
  traceWith stdoutTracer $ "Running ImmDB server at " ++ printHost (ip, port)
  startResourceTracer stdoutTracer rtsFrequency
  let remoteConfig = fmap (\url -> RemoteStorage.RemoteStorageConfig url remoteStorageCacheDir) remoteStorageSrcUrl
  absurd
    <$> ImmDBServer.run remoteConfig maxCachedChunks msgTracer eventTracer immDBDir sockAddr pInfoConfig

type RTSFrequency = Int

-- | Command-line options for the immdb-server.
data Opts = Opts
  { immDBDir :: FilePath
  -- ^ Local path to the ImmutableDB directory.
  , ip :: IPOctets
  -- ^ IP address to bind to.
  , port :: Socket.PortNumber
  -- ^ TCP port to listen on.
  , configFile :: FilePath
  -- ^ Path to the node configuration file.
  , rtsFrequency :: RTSFrequency
  -- ^ Frequency for tracing RTS statistics.
  , remoteStorageCacheDir :: String
  -- ^ Location of Sync Accelerator cache.
  , remoteStorageSrcUrl :: Maybe String
  -- ^ Optional CDN URL for the Genesis Sync Accelerator.
  , maxCachedChunks :: Int
  -- ^ Maximum number of chunks to keep in cache.
  }

printHost :: (IPOctets, Socket.PortNumber) -> String
printHost (ip, port) = printIP ip ++ ":" ++ show port

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Serve an ImmutableDB via ChainSync and BlockFetch"

  parse = do
    immDBDir <-
      strOption $
        mconcat
          [ long "db"
          , help "Path to the ImmutableDB"
          , metavar "PATH"
          ]
    addr <-
      option (maybeReader parseIPOctets) $
        mconcat
          [ long "addr"
          , help "Address to serve at"
          , value $ fromIPTuple (127, 0, 0, 1)
          , showDefault
          ]
    port <-
      option auto $
        mconcat
          [ long "port"
          , help "Port to serve on"
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
    rtsFrequency <-
      option auto $
        mconcat
          [ long "rts-frequency"
          , help "Frequency (in milliseconds) to poll GHC RTS statistics"
          , value 1000
          , showDefault
          ]
    remoteStorageCacheDir <-
      strOption $
        mconcat
          [ long "rs-cache-url"
          , help "Path to possible cache dir for the Sync Accelerator"
          , value "/tmp/sync-accelerator/"
          , metavar "PATH"
          , showDefault
          ]
    remoteStorageSrcUrl <-
      optional $
        strOption $
          mconcat
            [ long "rs-src-url"
            , help "URL to a CDN serving ImmutableDB chunks (e.g. https://example.com/chain). If left empty, the sync accelerator is disabled."
            , metavar "URL"
            ]
    maxCachedChunks <-
      option auto $
        mconcat
          [ long "max-cached-chunks"
          , help "Maximum number of chunks to keep in cache"
          , value 10
          , showDefault
          ]
    pure Opts{immDBDir, ip = addr, port, configFile, rtsFrequency, remoteStorageCacheDir, remoteStorageSrcUrl, maxCachedChunks}
