{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Node.Leashing (
    TraceLeashingEvent(..)
  , leashingWatcher 
  ) where

import           Control.Monad (void, when)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, LeashingState)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.AnchoredFragment (sharedCandidatePrefix)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Watcher (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF


type LeashingFingerprint blk = ([(Int, ChainHash (HeaderWithTime blk))], Maybe (ChainHash (HeaderWithTime blk)))

data LeashingWatcherState blk = LeashingWatcherState
  { leashingState :: LeashingState blk
  , genesisLoEFrag :: Maybe (AnchoredFragment (HeaderWithTime blk))
  , curChain :: AnchoredFragment (HeaderWithTime blk)
  }

-- 1. We want the leashing to work all the time (any state of the genesis)
-- 2. We want to leverage the existing LoE mechanism which is enabled atm only with GenesisMode
--
-- Then there is a problem: two watchers that write to varLoEFrag
--
-- Solution: leashingWatcher also watches the varLoEFrag, allowing us to
-- recalculate the leashing intersection with LoE fragment when the gddWatcher updates it.

leashingWatcher ::
     forall m blk.
     ( IOLike m
     , Typeable blk
     , GetHeader blk
     )
  => Tracer m (TraceLeashingEvent blk)
  -> ChainDB m blk
  -> StrictTVar m (LeashingState blk) 
  -> StrictTVar m (Maybe (AnchoredFragment (HeaderWithTime blk)))
   -- ^ The Genesis LoE fragment.
  -> StrictTVar m (AnchoredFragment (HeaderWithTime blk))
   -- ^ The leashing LoE fragment. 
  -> Watcher m (LeashingWatcherState blk) (LeashingFingerprint blk)
leashingWatcher tracer chainDb varLeashingState varGenesisLoEFrag varLoEFrag =
    Watcher {
        wInitial = Nothing
      , wReader 
      , wFingerprint
      , wNotify
      }
  where
    wReader :: STM m (LeashingWatcherState blk)
    wReader = do
        leashingState  <- readTVar varLeashingState
        genesisLoEFrag <- readTVar varGenesisLoEFrag
        curChain       <- ChainDB.getCurrentChainWithTime chainDb
        pure LeashingWatcherState {..}

    wFingerprint ::
         LeashingWatcherState blk
      -> LeashingFingerprint blk
    wFingerprint LeashingWatcherState{..} = (Map.foldMapWithKey (\k f -> [(k, AF.headHash f)]) leashingState, AF.headHash <$> genesisLoEFrag)

    wNotify :: (LeashingWatcherState blk) -> m ()
    wNotify LeashingWatcherState{..} = do
        let
          leashingCandidates = Map.toList leashingState
          prefix = maybe curChain id genesisLoEFrag
          leashingLoE = fst $ sharedCandidatePrefix prefix leashingCandidates 

        oldLoEFrag <- atomically $ swapTVar varLoEFrag leashingLoE 
        -- The chain selection only depends on the LoE tip, so there
        -- is no point in retriggering it if the LoE tip hasn't changed.
        when (AF.headHash oldLoEFrag /= AF.headHash leashingLoE) $
          void $ ChainDB.triggerChainSelectionAsync chainDb

        traceWith tracer $ TraceLeashingDebug "updated"

data TraceLeashingEvent blk = TraceLeashingDebug String

deriving stock instance
  ( GetHeader blk, Show (Header blk)
  ) => Show (TraceLeashingEvent blk)

