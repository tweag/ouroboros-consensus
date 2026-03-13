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

import           Control.Monad (void)
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
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LeashId)

-- | We want to know if either
-- 1. Any lsqLeashing fragment was updated. The map contains information about active
-- `LocalStateQuery` servers that enabled leashing providing their fragment. 
-- 2. The genesis LoE fragment was updated.
type LsqLeashingFingerprint blk =
  (Map.Map LeashId (ChainHash (HeaderWithTime blk)), ChainDB.LoE (ChainHash (HeaderWithTime blk)))

data LsqLeashingWatcherState blk = LsqLeashingWatcherState
  { lsqLeashingState :: LsqLeashingState blk
  , genesisLoE :: ChainDB.LoE (AnchoredFragment (HeaderWithTime blk))
  , curChain :: AnchoredFragment (HeaderWithTime blk)
  }

-- | The watcher that watches lsqLeashingState and genesisLoE,
-- and calculates their intersection using `sharedCandidatePrefix`.
lsqLeashingWatcher ::
     forall m blk.
     ( IOLike m
     , Typeable blk
     , GetHeader blk
     )
  => Tracer m (TraceLsqLeashingEvent blk)
  -> Set LeashId
  -> ChainDB m blk
  -> STM m (LsqLeashingState blk) 
  -> STM m (ChainDB.LoE (AnchoredFragment (HeaderWithTime blk)))
   -- ^ The Genesis LoE fragment.
  -> StrictTVar m (ChainDB.LoE (AnchoredFragment (HeaderWithTime blk)))
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
        lsqLeashingState <- getLsqLeashingState
        genesisLoE <- getGenesisLoEFrag
        curChain <- ChainDB.getCurrentChainWithTime chainDb
        pure LsqLeashingWatcherState {..}

    wFingerprint ::
         LsqLeashingWatcherState blk
      -> LsqLeashingFingerprint blk
    wFingerprint LsqLeashingWatcherState{..} = (Map.map AF.headHash lsqLeashingState, AF.headHash <$> genesisLoE)

    wNotify :: (LsqLeashingWatcherState blk) -> m ()
    wNotify LsqLeashingWatcherState{..} = do
        let
          lsqLeashingCandidates = Map.toList lsqLeashingState
          prefix = case genesisLoE of
              ChainDB.LoEEnabled frag -> frag
              ChainDB.LoEDisabled -> curChain
          lsqLeashingFrag = fst $ sharedCandidatePrefix prefix lsqLeashingCandidates
          crucialLsqClientsArePresent = crucialLsqClients == (Set.fromList $ map fst lsqLeashingCandidates)
          newLoE =
            if Set.null crucialLsqClients
            then
              -- if there are no crucial lsq clients and leashing state is empty, return genesis LoE
              if Map.null lsqLeashingState
              then genesisLoE
              else ChainDB.LoEEnabled lsqLeashingFrag
            else
              -- there are crucial lsq clients,
              -- if they are present, we leash in usual way 
              -- otherwise return the anchor of the current chain to leash the node at immutable tip
              if crucialLsqClientsArePresent then ChainDB.LoEEnabled lsqLeashingFrag
              else ChainDB.LoEEnabled $ AF.Empty $ AF.castAnchor $ AF.anchor curChain

        oldLoE <- atomically $ swapTVar varLoEFrag newLoE
        -- The chain selection only depends on the LoE tip, so there
        -- is no point in retriggering it if the LoE tip hasn't changed.
        case (oldLoE, newLoE) of
          (ChainDB.LoEDisabled, ChainDB.LoEDisabled) ->
            -- no changes
            pure ()
          (ChainDB.LoEEnabled oldLoEFrag, ChainDB.LoEEnabled newLoEFrag)
            | (AF.headHash oldLoEFrag) /= (AF.headHash newLoEFrag) ->
              -- LoE fragment was changed
              void $ ChainDB.triggerChainSelectionAsync chainDb
            | otherwise ->
              -- no changes
              pure () 
          (_, _) ->
            -- LoE either was enabled or disabled
            void $ ChainDB.triggerChainSelectionAsync chainDb

        traceWith tracer $ TraceLsqLeashingDebug "updated"

data TraceLsqLeashingEvent blk = TraceLsqLeashingDebug String

deriving stock instance
  ( GetHeader blk, Show (Header blk)
  ) => Show (TraceLsqLeashingEvent blk)

