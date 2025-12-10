{-# LANGUAGE FlexibleContexts #-}

module Cardano.Node.Tracing
  ( simpleFormat, startResourceTracer, traceResources
  ) where

import Data.Text (pack)

import           Cardano.Logging.Resources
import Cardano.Logging.Trace (traceWith)
import Cardano.Logging.Types (Trace, FormattedMessage (FormattedHuman))

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync (link)
import           GHC.Conc (labelThread, myThreadId)

-- | Simply lift an ordinary message into a FormattedMessage.
simpleFormat :: String -> FormattedMessage
simpleFormat = FormattedHuman False . pack

traceResources :: Trace IO ResourceStats -> Trace IO FormattedMessage -> IO ()
traceResources statsTrace errorTrace = do
  -- If stats are available, trace them using the provided trace.
  maybeStats <- readResourceStats
  case maybeStats of 
    Just stats -> traceWith statsTrace stats
    Nothing -> traceWith errorTrace $ simpleFormat "No resource stats"
  

-- | Starts a background thread to periodically trace resource statistics.
-- The thread reads resource stats and traces them using the given tracer.
-- It is linked to the parent thread to ensure proper error propagation.
startResourceTracer
  :: Trace IO ResourceStats
  -> Trace IO FormattedMessage
  -> Int
  -> IO ()
startResourceTracer _ _ 0 = pure ()
startResourceTracer statsTrace errorTrace delayMilliseconds = do
    as <- async resourceThread
    link as
  where
    -- | The background thread that periodically traces resource stats.
    resourceThread :: IO ()
    resourceThread = do
      -- Label the thread for easier debugging and identification.
      myThreadId >>= flip labelThread "Resource Stats Tracer"
      forever $ do
        traceResources statsTrace errorTrace
        threadDelay (delayMilliseconds * 1000)
