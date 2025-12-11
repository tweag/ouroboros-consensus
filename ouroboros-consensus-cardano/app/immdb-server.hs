{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

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

import Cardano.Node.Tracing (
  getResourceTracer',
  startResourceTracer', 
  traceResources'',
  )
import "contra-tracer" Control.Tracer (stdoutTracer, traceWith)
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
  traceWith stdoutTracer $ "Running ImmDB server at " ++ printHost (addr, port)
  traceWith stdoutTracer "Polling resources"
  traceResources'' stdoutTracer (getResourceTracer' stdoutTracer)
  startResourceTracer' stdoutTracer rtsFrequency
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
          , help "Frequency (in milliseconds) to poll GHC RTS statistics"
          , value 1000
          , showDefault
          ]
    pure Opts{immDBDir, addr, port, configFile, rtsFrequency}
