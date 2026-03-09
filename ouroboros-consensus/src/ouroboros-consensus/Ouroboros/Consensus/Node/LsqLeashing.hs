{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Node.LsqLeashing (
    TraceLsqLeashingEvent(..)
  , lsqLeashingWatcher 
  ) where

import           Control.Monad (void, when)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, LsqLeashingState)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.AnchoredFragment (sharedCandidatePrefix)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.STM (Watcher (..))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LeashID)

-- | We want to know if either
-- 1. Any lsqLeashing fragment was updated. The map contains information about active
-- `LocalStateQuery` servers that enabled leashing providing their fragment. 
-- 2. The genesis LoE fragment was updated.
type LsqLeashingFingerprint blk =
  (Map.Map LeashID (ChainHash (HeaderWithTime blk)), Maybe (ChainHash (HeaderWithTime blk)))

data LsqLeashingWatcherState blk = LsqLeashingWatcherState
  { lsqLeashingState :: LsqLeashingState blk
  , genesisLoEFrag :: Maybe (AnchoredFragment (HeaderWithTime blk))
  , curChain :: AnchoredFragment (HeaderWithTime blk)
  }

-- | The watcher that watches lsqLeashingState and genesisLoEFrag,
-- and calculates their intersection using `sharedCandidatePrefix`.
lsqLeashingWatcher ::
     forall m blk.
     ( IOLike m
     , Typeable blk
     , GetHeader blk
     )
  => Tracer m (TraceLsqLeashingEvent blk)
  -> Set LeashID
  -> ChainDB m blk
  -> STM m (LsqLeashingState blk) 
  -> STM m (Maybe (AnchoredFragment (HeaderWithTime blk)))
   -- ^ The Genesis LoE fragment.
  -> StrictTVar m (AnchoredFragment (HeaderWithTime blk))
   -- ^ The resulting leashing LoE fragment. 
  -> Watcher m (LsqLeashingWatcherState blk) (LsqLeashingFingerprint blk)
lsqLeashingWatcher tracer crucialLsqClients chainDb getLsqLeashingState getGenesisLoEFrag varLoEFrag =
    Watcher {
        wInitial = Nothing
      , wReader 
      , wFingerprint
      , wNotify
      }
  where
    wReader :: STM m (LsqLeashingWatcherState blk)
    wReader = do
        lsqLeashingState  <- getLsqLeashingState
        genesisLoEFrag <- getGenesisLoEFrag
        curChain       <- ChainDB.getCurrentChainWithTime chainDb
        pure LsqLeashingWatcherState {..}

    wFingerprint ::
         LsqLeashingWatcherState blk
      -> LsqLeashingFingerprint blk
    wFingerprint LsqLeashingWatcherState{..} = (Map.map AF.headHash lsqLeashingState, AF.headHash <$> genesisLoEFrag)

    wNotify :: (LsqLeashingWatcherState blk) -> m ()
    wNotify LsqLeashingWatcherState{..} = do
        let
          lsqLeashingCandidates = Map.toList lsqLeashingState
          prefix = maybe curChain id genesisLoEFrag
          crucialLsqClientsArePresent = crucialLsqClients == (Set.fromList $ map fst lsqLeashingCandidates)
          loeFrag =
            if Set.null crucialLsqClients || crucialLsqClientsArePresent
            then fst $ sharedCandidatePrefix prefix lsqLeashingCandidates
            else AF.Empty $ AF.castAnchor $ AF.anchor curChain

        oldLoEFrag <- atomically $ swapTVar varLoEFrag loeFrag 
        -- The chain selection only depends on the LoE tip, so there
        -- is no point in retriggering it if the LoE tip hasn't changed.
        when ((AF.headHash oldLoEFrag) /= (AF.headHash loeFrag)) $
          void $ ChainDB.triggerChainSelectionAsync chainDb

        traceWith tracer $ TraceLsqLeashingDebug "updated"

data TraceLsqLeashingEvent blk = TraceLsqLeashingDebug String

deriving stock instance
  ( GetHeader blk, Show (Header blk)
  ) => Show (TraceLsqLeashingEvent blk)

