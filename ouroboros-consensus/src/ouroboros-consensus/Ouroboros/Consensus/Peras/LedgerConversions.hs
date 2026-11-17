{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Conversions from the ledger types to their mirrors in
-- @Cardano.Crypto.Peras.Committee.Base@, which the voting committee code of
-- @cardano-crypto-peras@ uses.
module Ouroboros.Consensus.Peras.LedgerConversions
  ( toCommitteeNonce
  , toCommitteePoolId
  , fromCommitteePoolId
  , toCommitteeBlsKey
  ) where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Peras.Committee.Base as Committee
import Cardano.Crypto.Peras.Committee.Types (PoolId (..))
import Cardano.Ledger.BaseTypes (Nonce (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (StakePool))
import Cardano.Ledger.State (BlsKey (..))

toCommitteeNonce :: Nonce -> Committee.Nonce
toCommitteeNonce = \case
  NeutralNonce -> Committee.NeutralNonce
  Nonce h -> Committee.Nonce (Hash.castHash h)

-- | Preserves the order of the pool ids.
toCommitteePoolId :: KeyHash StakePool -> PoolId
toCommitteePoolId (KeyHash h) = PoolId (Committee.KeyHash h)

fromCommitteePoolId :: PoolId -> KeyHash StakePool
fromCommitteePoolId (PoolId (Committee.KeyHash h)) = KeyHash h

toCommitteeBlsKey :: BlsKey -> Committee.BlsKey
toCommitteeBlsKey key = Committee.BlsKey (blsPubKey key) (blsPossessionProof key)
