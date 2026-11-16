{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | What the ledger needs from consensus to validate the Peras certificate of
-- a block.
--
-- The ledger validates a certificate against the voting committee of the epoch
-- of its round, which is the epoch of the block or the one before it. It has
-- the stake distributions of both epochs, but neither their nonces nor which
-- rounds they span: consensus supplies these, from the header state and from
-- the Peras epoch context resolver, when it applies the block.
module Ouroboros.Consensus.Peras.ApplyContext
  ( PerasApplyContext (..)
  , PerasApplyEpoch (..)
  ) where

import Cardano.Ledger.BaseTypes (Nonce, StrictMaybe)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)

-- | The epochs whose certificates a block may carry.
data PerasApplyContext = PerasApplyContext
  { pacEpoch :: !PerasApplyEpoch
  -- ^ The epoch of the block
  , pacPrevEpoch :: !(StrictMaybe PerasApplyEpoch)
  -- ^ The epoch before it, if Peras was enabled in it
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks

-- | The nonce and rounds of an epoch.
data PerasApplyEpoch = PerasApplyEpoch
  { paeNonce :: !Nonce
  -- ^ Epoch nonce
  , paeStartRound :: !PerasRoundNo
  -- ^ First round of the epoch (inclusive)
  , paeEndRound :: !PerasRoundNo
  -- ^ First round of the next epoch (exclusive bound)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NoThunks
