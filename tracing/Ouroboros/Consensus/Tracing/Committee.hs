{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Tracing.Committee () where

import Cardano.Logging (LogFormatting (..))
import Data.Aeson (Value (..), (.=))
import Cardano.Crypto.Peras.Committee.Class (VotingCommitteeError)
import Cardano.Crypto.Peras.Committee.EveryoneVotes (EveryoneVotes)
import qualified Cardano.Crypto.Peras.Committee.EveryoneVotes as EveryoneVotes
import Cardano.Crypto.Peras.Committee.Types (TargetCommitteeSize (..))
import Cardano.Crypto.Peras.Committee.WFA
  ( NumPoolsWithPositiveStake (..)
  , SeatIndex (..)
  , WFAError (..)
  )
import Cardano.Crypto.Peras.Committee.WFALS (WFALS)
import qualified Cardano.Crypto.Peras.Committee.WFALS as WFALS
import Ouroboros.Consensus.Peras.LedgerConversions (fromCommitteePoolId)

--
-- instances for WFAError
--

instance LogFormatting WFAError where
  forMachine _dtal EmptyStakeDistribution =
    mconcat
      [ "kind" .= String "EmptyStakeDistribution"
      ]
  forMachine _dtal (NotEnoughPoolsWithPositiveStake targetCommitteeSize numPoolsWithPositiveStake) =
    mconcat
      [ "kind" .= String "NotEnoughPoolsWithPositiveStake"
      , "targetCommitteeSize" .= unTargetCommitteeSize targetCommitteeSize
      , "numPoolsWithPositiveStake" .= unNumPoolsWithPositiveStake numPoolsWithPositiveStake
      ]

instance LogFormatting (VotingCommitteeError crypto WFALS) where
  forMachine _dtal (WFALS.WFAError e) =
    mconcat
      [ "kind" .= String "WFAError"
      , forMachine _dtal e
      ]
  forMachine _dtal (WFALS.MissingPoolId poolId) =
    mconcat
      [ "kind" .= String "MissingPoolId"
      , "poolId" .= fromCommitteePoolId poolId
      ]
  forMachine _dtal (WFALS.NotAPersistentMember seatIndex) =
    mconcat
      [ "kind" .= String "NotAPersistentMember"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (WFALS.NotANonPersistentMember seatIndex) =
    mconcat
      [ "kind" .= String "NotANonPersistentMember"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (WFALS.ZeroNonPersistentSeats seatIndex) =
    mconcat
      [ "kind" .= String "ZeroNonPersistentSeats"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (WFALS.InvalidVoteSignature msg) =
    mconcat
      [ "kind" .= String "InvalidVoteSignature"
      , "message" .= msg
      ]
  forMachine _dtal (WFALS.InvalidVoterEligibilityProof msg) =
    mconcat
      [ "kind" .= String "InvalidVoterEligibilityProof"
      , "message" .= msg
      ]
  forMachine _dtal (WFALS.InvalidCertSignature msg) =
    mconcat
      [ "kind" .= String "InvalidCertSignature"
      , "message" .= msg
      ]
  forMachine _dtal (WFALS.CryptoError msg) =
    mconcat
      [ "kind" .= String "CryptoError"
      , "message" .= msg
      ]

instance LogFormatting (VotingCommitteeError crypto EveryoneVotes) where
  forMachine _dtal (EveryoneVotes.WFAError e) =
    mconcat
      [ "kind" .= String "WFAError"
      , forMachine _dtal e
      ]
  forMachine _dtal (EveryoneVotes.MissingPoolId poolId) =
    mconcat
      [ "kind" .= String "MissingPoolId"
      , "poolId" .= fromCommitteePoolId poolId
      ]
  forMachine _dtal (EveryoneVotes.MissingSeatIndex seatIndex) =
    mconcat
      [ "kind" .= String "MissingSeatIndex"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (EveryoneVotes.PoolHasNoStake seatIndex) =
    mconcat
      [ "kind" .= String "PoolHasNoStake"
      , "seatIndex" .= unSeatIndex seatIndex
      ]
  forMachine _dtal (EveryoneVotes.InvalidVoteSignature msg) =
    mconcat
      [ "kind" .= String "InvalidVoteSignature"
      , "message" .= msg
      ]
  forMachine _dtal (EveryoneVotes.InvalidCertSignature msg) =
    mconcat
      [ "kind" .= String "InvalidCertSignature"
      , "message" .= msg
      ]
  forMachine _dtal (EveryoneVotes.CryptoError msg) =
    mconcat
      [ "kind" .= String "CryptoError"
      , "message" .= msg
      ]
