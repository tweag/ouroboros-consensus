{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Peras.Voting.V1
  ( PerasVotingCommitteeScheme
  , mkPerasVotingCommitteeInput

    -- * For testing purposes
  , extractPerasStakeDistrAndPublicKeys
  , ledgerKeyScope
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Base (Any)
import Ouroboros.Consensus.Block.SupportsPeras (PerasCrypto)
import Cardano.Crypto.Peras.Committee.Class (CryptoSupportsVotingCommittee (..))
import qualified Cardano.Crypto.Peras.Committee.Crypto.BLS as BLS
import Cardano.Crypto.Peras.Committee.Types
  ( LedgerStake (..)
  , PoolId
  , TargetCommitteeSize
  )
import Cardano.Crypto.Peras.Committee.WFA
  ( mkExtWFAStakeDistr
  , wFATiebreakerWithEpochNonce
  )
import Cardano.Crypto.Peras.Committee.WFALS (VotingCommitteeInput (..), WFALS)
import Cardano.Crypto.Peras.Crypto.BLS (PerasBLSCrypto, PerasPublicKey (..))
import qualified Ouroboros.Consensus.Peras.Error.V1 as V1
import Ouroboros.Consensus.Peras.LedgerConversions
  ( toCommitteeBlsKey
  , toCommitteeNonce
  , toCommitteePoolId
  )

type PerasVotingCommitteeScheme = WFALS

-- | Key scope used for deriving the BLS public keys from the ledger.
ledgerKeyScope :: BLS.KeyScope
ledgerKeyScope = "PERAS/LEDGER"

extractPerasStakeDistrAndPublicKeys ::
  PoolDistr ->
  Map PoolId (LedgerStake, PerasPublicKey)
extractPerasStakeDistrAndPublicKeys =
  Map.mapKeysMonotonic toCommitteePoolId
    . Map.mapMaybe extractEntry
    . unPoolDistr
 where
  extractEntry poolStake =
    case individualPoolStakeBls poolStake of
      SNothing ->
        Nothing
      SJust blsKey ->
        Just
          ( LedgerStake
              . individualPoolStake
              $ poolStake
          , PerasPublicKey
              . BLS.coercePublicKey @Any
              . BLS.publicKeyFromLedgerBlsKey ledgerKeyScope
              $ toCommitteeBlsKey blsKey
          )

mkPerasVotingCommitteeInput ::
  forall blk.
  PerasCrypto blk ~ PerasBLSCrypto =>
  PoolDistr ->
  Nonce ->
  TargetCommitteeSize ->
  Either (V1.PerasError blk) (VotingCommitteeInput (PerasCrypto blk) WFALS)
mkPerasVotingCommitteeInput poolDistr epochNonce targetCommitteeSize = do
  extWFAStakeDistr <-
    bimap V1.PerasVotingWFAError id $
      mkExtWFAStakeDistr
        (wFATiebreakerWithEpochNonce (toCommitteeNonce epochNonce))
        (extractPerasStakeDistrAndPublicKeys poolDistr)
  pure $
    WFALSVotingCommitteeInput
      (toCommitteeNonce epochNonce)
      targetCommitteeSize
      extWFAStakeDistr
