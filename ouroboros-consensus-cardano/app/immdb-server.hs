{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import qualified Cardano.Tools.ImmDBServer.Diffusion as ImmDBServer
import Data.Bifunctor (first)
import Data.Void
import Data.Word (Word8)
import Main.Utf8 (withStdTerminalHandles)
import qualified Network.Socket as Socket
import Options.Applicative
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Text.Read (readMaybe)

import Cardano.Node.Tracing (startResourceTracer, simpleFormat, traceResources)
import Cardano.Logging (mkCardanoTracer, configureTracers, emptyConfigReflection, TraceConfig, Trace)
import Cardano.Logging.Resources.Types (ResourceStats)
import Cardano.Logging.Trace (traceWith)
import Cardano.Logging.Tracer.Standard (standardTracer)
import Cardano.Logging.Types (FormattedMessage, Trace(..), TraceConfig(..), emptyTraceConfig)
import Control.Tracer (nullTracer)
import Data.List (intercalate)

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  Opts{immDBDir, addr, port, configFile, rtsFrequency} <- execParser optsParser
  let sockAddr = Socket.SockAddrInet port hostAddr
       where
        -- could also be passed in
        hostAddr = Socket.tupleToHostAddress addr
      args = Cardano.CardanoBlockArgs configFile Nothing
  ProtocolInfo{pInfoConfig} <- mkProtocolInfo args
  
  stdoutTrace <- standardTracer
  resourceTrace <- getResourceTrace rtsFrequency stdoutTrace
  traceResources resourceTrace stdoutTrace

  traceWith stdoutTrace $ simpleFormat ("ImmDB server running on " ++ printHost (addr, port))
  startResourceTracer resourceTrace stdoutTrace rtsFrequency
  absurd <$> ImmDBServer.run immDBDir sockAddr pInfoConfig

type HostAddr = (Word8, Word8, Word8, Word8)
type RTSFrequency = Int

data Opts = Opts
  { immDBDir :: FilePath
  , addr :: HostAddr
  , port :: Socket.PortNumber
  , configFile :: FilePath
  , rtsFrequency :: RTSFrequency
  }

getTraceConfig :: RTSFrequency -> TraceConfig
getTraceConfig rtsFrequency = emptyTraceConfig{
  tcPeerFrequency = Nothing,
  tcResourceFrequency = Just rtsFrequency,
  tcLedgerMetricsFrequency = Nothing
}

getResourceTrace :: RTSFrequency -> Trace IO FormattedMessage -> IO (Trace IO ResourceStats)
getResourceTrace freq trBase = do
  tr <- mkCardanoTracer trBase (Trace nullTracer) Nothing []
  -- cr <- emptyConfigReflection
  -- configureTracers cr (getTraceConfig freq) [tr]
  return tr

printHost :: (HostAddr, Socket.PortNumber) -> String
printHost ((a, b, c, d), port) = intercalate "." subs ++ ":" ++ show port
 where
  subs = map show [a, b, c, d]

parseAddr :: String -> Either String HostAddr
parseAddr s = first contextualize $ traverse tryParse chunks >>= extractResult
 where
  contextualize msg = "Cannot parse address (" ++ s ++ "): " ++ msg
  chunks = reverse $ map reverse $ go s [] []
  go [] curr acc = curr : acc
  go ('.' : t) curr acc = go t [] (curr : acc)
  go (h : t) curr acc = go t (h : curr) acc
  tryParse sub = maybe (Left ("cannot parse component '" ++ sub ++ "'")) Right (readMaybe sub)
  extractResult [sub1, sub2, sub3, sub4] = Right (sub1, sub2, sub3, sub4)
  extractResult subs = Left (show (length subs) ++ " (not 4) components")

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
      option (eitherReader parseAddr) $
        mconcat
          [ long "addr"
          , help "Address to serve at"
          , value (127, 0, 0, 1)
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
          , help "Frequency (in milliseconds) for reporting GHC runtime system (RTS) info"
          , value 1000 -- 1 second
          , showDefault
          ]
    pure Opts{immDBDir, addr, port, configFile, rtsFrequency}
