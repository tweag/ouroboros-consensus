{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing
  ( getResourceTracer
  , startResourceTracer
  , traceResources
  ) where

import Cardano.Logging.Resources (ResourceStats, readResourceStats)
import Cardano.Logging.Types (LogFormatting (..))
import Data.Text (unpack)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Monad.Class.MonadAsync (link)
import GHC.Conc (labelThread, myThreadId)
import "contra-tracer" Control.Tracer (Tracer, contramap, traceWith)

-- | Starts a background thread to periodically trace resource statistics.
-- The thread reads resource stats and traces them using the given tracer.
-- It is linked to the parent thread to ensure proper error propagation.
startResourceTracer :: Tracer IO String -> Int -> IO ()
startResourceTracer _ 0 = pure ()
startResourceTracer trBase delayMilliseconds = do
  as <- async resourceThread
  link as
 where
  trStats = getResourceTracer trBase
  -- \| The background thread that periodically traces resource stats.
  resourceThread :: IO ()
  resourceThread = do
    -- Label the thread for easier debugging and identification.
    myThreadId >>= flip labelThread "Resource Stats Tracer"
    forever $ do
      traceResources trBase trStats
      threadDelay (delayMilliseconds * 1000)

-- | Obtain a Tracer for a domain-specific type by converting it to text.
getResourceTracer :: Tracer IO String -> Tracer IO ResourceStats
getResourceTracer = contramap (unpack . forHumanFromMachine)

-- | Attempt to read GHC RTS statistics, and log them if available.
traceResources :: Tracer IO String -> Tracer IO ResourceStats -> IO ()
traceResources trError trStats = do
  maybeStats <- readResourceStats
  case maybeStats of
    Nothing -> traceWith trError "No resource stats available"
    Just stats -> traceWith trStats stats
