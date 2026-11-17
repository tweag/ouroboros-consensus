{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test utilities for the V1 (production) Peras implementation.
module Test.Util.Peras.V1
  ( perasVoteIsPersistent
  , perasCertContainsOnlyPersistentVotes
  , genPerasVote
  , genPerasCert
  , genPrivateKey
  , tabulatePerasCert
  , tabulatePerasVote
  ) where

import Cardano.Crypto.Hash (ByteString)
import qualified Data.ByteString as ByteString
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Word (Word8)
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( VRFOutput (..)
  , VoteSignature (..)
  )
import Ouroboros.Consensus.Peras.Types (PerasBoostedBlock)
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1
import qualified Test.Cardano.Crypto.Peras.Gen as Gen
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , frequency
  , tabulate
  , vectorOf
  )
import Test.Util.Peras.Common
  ( genRoundNo
  , genSeatIndex
  , mkBucket
  )

-- | Whether a Peras vote is a persistent one
perasVoteIsPersistent :: V1.PerasVote tag -> Bool
perasVoteIsPersistent vote
  | V1.PersistentPerasVoteEligibilityProof{} <- V1.pvEligibilityProof vote = True
  | otherwise = False

-- | Whether a Peras certificate only contains persistent votes
perasCertContainsOnlyPersistentVotes :: V1.PerasCert tag -> Bool
perasCertContainsOnlyPersistentVotes cert =
  V1.perasCertNumberOfNonPersistentVoters cert == 0

genBoostedBlock :: Gen PerasBoostedBlock
genBoostedBlock = Gen.genPerasBoostedBlock

genPrivateKey :: Proxy r -> Gen (BLS.PrivateKey r)
genPrivateKey _ =
  fromMaybe (error "genPrivateKey: invalid key bytes")
    . BLS.rawDeserialisePrivateKey "ROUNDTRIP"
    . ByteString.pack
    <$> vectorOf 32 (arbitrary @Word8)

genSignature :: forall r. BLS.HasBLSContext r => Proxy r -> Gen (BLS.Signature r)
genSignature _ = do
  key <- genPrivateKey (Proxy @r)
  msg <- fromString @ByteString <$> arbitrary
  pure $ BLS.signWithRole key msg

genVoteEligibilityProof :: Bool -> Gen V1.PerasVoteEligibilityProof
genVoteEligibilityProof shouldGenNonPersistent = do
  frequency
    [
      ( 4
      , pure V1.PersistentPerasVoteEligibilityProof
      )
    ,
      ( if shouldGenNonPersistent then 1 else 0
      , V1.NonPersistentPerasVoteEligibilityProof
          . PerasBLSCryptoVRFOutput
          <$> genSignature (Proxy @BLS.VRF)
      )
    ]

genPerasVote :: Bool -> Gen (V1.PerasVote tag)
genPerasVote shouldGenNonPersistent = do
  pvRoundNo <- genRoundNo
  pvBoostedBlock <- genBoostedBlock
  pvSeatIndex <- genSeatIndex
  pvEligibilityProof <- genVoteEligibilityProof shouldGenNonPersistent
  pvSignature <-
    PerasBLSCryptoVoteSignature
      <$> genSignature (Proxy @BLS.SIGN)
  pure
    V1.PerasVote
      { V1.pvRoundNo
      , V1.pvBoostedBlock
      , V1.pvSeatIndex
      , V1.pvEligibilityProof
      , V1.pvSignature
      }

genPerasCert :: Bool -> Gen (V1.PerasCert tag)
genPerasCert shouldGenNonPersistent =
  V1.PerasCert <$> Gen.genPerasCert shouldGenNonPersistent

tabulatePerasCert :: V1.PerasCert tag -> Property -> Property
tabulatePerasCert cert =
  foldr (flip (.)) id $
    [ tabulate
        "Number of voters"
        [mkBucket 100 numVoters " voters"]
    , tabulate
        "Proportion of persistent voters"
        [mkBucket 10 persistentVotersRatio "%"]
    ]
 where
  numVoters = V1.perasCertNumberOfVoters cert
  numPersistentVoters = numVoters - V1.perasCertNumberOfNonPersistentVoters cert

  persistentVotersRatio
    | numVoters == 0 = 0
    | otherwise = numPersistentVoters * 100 `div` numVoters

tabulatePerasVote :: V1.PerasVote tag -> Property -> Property
tabulatePerasVote vote =
  foldr (flip (.)) id $
    [ tabulate
        "Voter type"
        [voterType]
    ]
 where
  voterType =
    case V1.pvEligibilityProof vote of
      V1.PersistentPerasVoteEligibilityProof -> "persistent"
      V1.NonPersistentPerasVoteEligibilityProof _ -> "non-persistent"
