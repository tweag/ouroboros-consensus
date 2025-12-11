{-# LANGUAGE PackageImports #-}

module Cardano.Node.Tracing
  ( getResourceTracer', simpleFormat, startResourceTracer, startResourceTracer', traceResources, traceResources', traceResources''
  ) where

import           Cardano.Logging.Resources (ResourceStats, readResourceStats)
import Cardano.Logging.Types (FormattedMessage (..), LogFormatting (..))
import Data.Text (pack, unpack)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync (link)
import           "contra-tracer" Control.Tracer (Tracer, contramap, traceWith)
import           GHC.Conc (labelThread, myThreadId)


startResourceTracer' :: Tracer IO String -> Int -> IO ()
startResourceTracer' _ 0 = pure ()
startResourceTracer' trBase delayMilliseconds = do
    as <- async resourceThread
    link as
  where
    trStats = getResourceTracer' trBase
    -- | The background thread that periodically traces resource stats.
    resourceThread :: IO ()
    resourceThread = do
      -- Label the thread for easier debugging and identification.
      myThreadId >>= flip labelThread "Resource Stats Tracer"
      forever $ do
        traceResources'' trBase trStats
        threadDelay (delayMilliseconds * 1000)

-- | Starts a background thread to periodically trace resource statistics.
-- The thread reads resource stats and traces them using the given tracer.
-- It is linked to the parent thread to ensure proper error propagation.
startResourceTracer
  :: Tracer IO ResourceStats
  -> Tracer IO FormattedMessage
  -> Int
  -> IO ()
startResourceTracer _ _ 0 = pure ()
startResourceTracer traceStats traceError delayMilliseconds = do
    as <- async resourceThread
    link as
  where
    -- | The background thread that periodically traces resource stats.
    resourceThread :: IO ()
    resourceThread = do
      -- Label the thread for easier debugging and identification.
      myThreadId >>= flip labelThread "Resource Stats Tracer"
      forever $ do
        traceResources traceStats traceError
        threadDelay (delayMilliseconds * 1000)

getResourceTracer' :: Tracer IO String -> Tracer IO ResourceStats
getResourceTracer' = contramap (unpack . forHumanFromMachine)

getResourceTracer :: Tracer IO FormattedMessage -> Tracer IO ResourceStats
getResourceTracer = contramap (FormattedHuman False . forHumanFromMachine)

traceResources'' :: Tracer IO String -> Tracer IO ResourceStats -> IO ()
traceResources'' trError trStats = do
  maybeStats <- readResourceStats
  case maybeStats of
    Nothing -> traceWith trError "No resource stats available"
    Just stats -> traceWith trStats stats  

traceResources' :: Tracer IO FormattedMessage -> IO ()
traceResources' tr = 
  do
    maybeStats <- readResourceStats
    case maybeStats of
      Nothing -> traceWith tr $ simpleFormat "No resource stats available"
      Just stats -> traceWith (getResourceTracer tr) stats

traceResources :: Tracer IO ResourceStats -> Tracer IO FormattedMessage -> IO ()
traceResources trStats trError = do
  maybeStats <- readResourceStats
  case maybeStats of
    Nothing -> traceWith trError $ simpleFormat "No resource stats available"
    Just stats -> traceWith trStats stats

simpleFormat :: String -> FormattedMessage
simpleFormat = FormattedHuman False . pack
