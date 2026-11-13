{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Concrete Peras certificate types using BLS signatures.
--
-- NOTE: this module is meant to be imported qualified.
--
-- NOTE: the validation performed during serialization is minimal, and does not
-- cover any of additional semantic and cryptographic checks that must be
-- performed on the certificate later on.
module Ouroboros.Consensus.Peras.Cert.V1
  ( PerasCert (..)
  , castPerasCert
  , perasCertSizeUpperBound

    -- * Exported for testing purposes only
  , perasCertNumberOfNonPersistentVoters
  , perasCertNumberOfVoters
  , PerasCertSize
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeBytes, decodeListLenOf, encodeBytes, encodeListLen)
import qualified Cardano.Crypto.Peras.Cert as Base
import Data.Array.Byte (ByteArray (..))
import Data.Bitmap (Bitmap (..))
import Data.ByteString.Short (ShortByteString (SBS))
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsVRF (..)
  )
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( Cert (..)
  , EveryoneVotes
  )
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Committee.WFALS (Cert (..), WFALS)
import Ouroboros.Consensus.Peras.Cert.Class (IsPerasCert (..))
import Ouroboros.Consensus.Peras.Crypto.BLS
  ( PerasBLSCrypto
  , PerasBLSCryptoAggregateVoteSignature (..)
  , VRFOutput (..)
  )
import Ouroboros.Consensus.Peras.Types
  ( BoostedBlock
  , PerasBoostedBlock
  , PerasCertSize
  )
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasConversionError (..)
  , fromPerasSeatIndex
  , toPerasSeatIndex
  )
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

-- | Concrete Peras certificates using BLS signatures
--
-- NOTE: the 'blk' parameter is a phantom type used to track the block type that
-- the certificate is associated with, to ensure injectivity when 'V1.PerasCert'
-- is used as a type instance for 'BlockSupportsPeras' class.
newtype PerasCert blk
  = PerasCert
  { unPerasCert :: Base.PerasCert
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype NoThunks

instance ToCBOR Base.PerasVotersBitmap where
  toCBOR (Base.UnsafePerasVotersBitmap maxIx bs) =
    encodeListLen 2
      <> toCBOR maxIx
      <> encodeBytes (case bitmapBytes bs of ByteArray ba -> SBS.fromShort (SBS ba))

instance FromCBOR Base.PerasVotersBitmap where
  fromCBOR = do
    decodeListLenOf 2
    maxIx <- fromCBOR
    bs <- decodeBytes
    case Base.mkPerasVotersBitmap maxIx (Bitmap (case SBS.toShort bs of SBS ba -> ByteArray ba)) of
      Nothing ->
        fail "Bitmap: invalid bitmap data or size mismatch"
      Just bitmap ->
        pure bitmap

instance ToCBOR Base.PerasCertVoters where
  toCBOR (Base.UnsafePerasCertVoters bitmap vrfOutputs) =
    encodeListLen 2
      <> toCBOR bitmap
      <> toCBOR vrfOutputs

instance FromCBOR Base.PerasCertVoters where
  fromCBOR = do
    decodeListLenOf 2
    bitmap <- fromCBOR
    vrfOutputs <- fromCBOR
    either fail pure (Base.mkPerasCertVoters bitmap vrfOutputs)

instance ToCBOR Base.PerasCert where
  toCBOR (Base.PerasCert roundNo boostedBlock voters signature) =
    encodeListLen 4
      <> toCBOR roundNo
      <> toCBOR boostedBlock
      <> toCBOR voters
      <> toCBOR signature

instance FromCBOR Base.PerasCert where
  fromCBOR = do
    decodeListLenOf 4
    Base.PerasCert
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

deriving newtype instance Typeable blk => ToCBOR (PerasCert blk)

deriving newtype instance Typeable blk => FromCBOR (PerasCert blk)

-- | Retag a 'PerasCert' with a new phantom @blk@ type.
castPerasCert :: forall blk' blk. PerasCert blk -> PerasCert blk'
castPerasCert = coerce

type instance BoostedBlock (PerasCert blk) = PerasBoostedBlock

instance
  ( ConvertRawHash blk
  , HashSize blk ~ 32
  ) =>
  IsPerasCert (PerasCert blk) blk
  where
  getPerasCertRound = Base.pcRoundNo . unPerasCert
  getPerasCertBlock = Base.pcBoostedBlock . unPerasCert

instance ShowProxy blk => ShowProxy (PerasCert blk) where
  showProxy _ = "PerasCert " <> showProxy (Proxy @blk)

perasCertSizeUpperBound :: PerasCert blk -> PerasCertSize
perasCertSizeUpperBound = Base.perasCertSizeUpperBound . unPerasCert

perasCertNumberOfNonPersistentVoters :: PerasCert blk -> Int
perasCertNumberOfNonPersistentVoters =
  Base.perasCertNumberOfNonPersistentVoters . Base.pcVoters . unPerasCert

perasCertNumberOfVoters :: PerasCert blk -> Int
perasCertNumberOfVoters =
  Base.perasCertNumberOfVoters . Base.pcVoters . unPerasCert

-- * Compatibility with voting committee implementations

toVRFOutput :: Base.PerasVRFOutput -> VRFOutput PerasBLSCrypto
toVRFOutput = PerasBLSCryptoVRFOutput . BLS.mkSignature . Base.unPerasVRFOutput

fromVRFOutput :: VRFOutput PerasBLSCrypto -> Base.PerasVRFOutput
fromVRFOutput = Base.PerasVRFOutput . BLS.rawSignature . unPerasBLSCryptoVRFOutput

toAggregateSignature :: Base.PerasSignature -> AggregateVoteSignature PerasBLSCrypto
toAggregateSignature =
  PerasBLSCryptoAggregateVoteSignature . BLS.mkSignature . Base.unPerasSignature

fromAggregateSignature :: AggregateVoteSignature PerasBLSCrypto -> Base.PerasSignature
fromAggregateSignature =
  Base.PerasSignature . BLS.rawSignature . unPerasBLSCryptoAggregateVoteSignature

fromPerasCertVoters ::
  Base.PerasCertVoters ->
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto)))
fromPerasCertVoters =
  NEMap.fromAscList
    . NonEmpty.map (\(seat, proof) -> (fromPerasSeatIndex seat, toVRFOutput <$> proof))
    . Base.perasCertVotersToSeats

toPerasCertVoters ::
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto))) ->
  Either PerasConversionError Base.PerasCertVoters
toPerasCertVoters voters = do
  seats <-
    traverse
      ( \(seat, proof) -> do
          seat' <- toPerasSeatIndex seat
          pure (seat', fromVRFOutput <$> proof)
      )
      (NEMap.toAscList voters)
  either (Left . PerasCertVotersError) pure (Base.perasCertVotersFromSeats seats)

mkPerasCert ::
  Base.PerasRoundNo ->
  PerasBoostedBlock ->
  Base.PerasCertVoters ->
  AggregateVoteSignature PerasBLSCrypto ->
  PerasCert blk
mkPerasCert roundNo boostedBlock voters sig =
  PerasCert
    Base.PerasCert
      { Base.pcRoundNo = roundNo
      , Base.pcBoostedBlock = boostedBlock
      , Base.pcVoters = voters
      , Base.pcSignature = fromAggregateSignature sig
      }

instance
  PerasCertCompatibleWithVotingCommittee
    (PerasCert blk)
    PerasBLSCrypto
    WFALS
  where
  toPerasCert = \case
    WFALSCert electionId candidate voters sig -> do
      voters' <- toPerasCertVoters voters
      pure $ mkPerasCert electionId candidate voters' sig

  fromPerasCert = \case
    PerasCert Base.PerasCert{Base.pcRoundNo, Base.pcBoostedBlock, Base.pcVoters, Base.pcSignature} ->
      pure $
        WFALSCert
          pcRoundNo
          pcBoostedBlock
          (fromPerasCertVoters pcVoters)
          (toAggregateSignature pcSignature)

instance
  PerasCertCompatibleWithVotingCommittee
    (PerasCert blk)
    PerasBLSCrypto
    EveryoneVotes
  where
  toPerasCert = \case
    EveryoneVotesCert electionId candidate voters sig -> do
      voters' <- toPerasCertVoters (NEMap.fromSet (const Nothing) voters)
      pure $ mkPerasCert electionId candidate voters' sig

  fromPerasCert = \case
    PerasCert Base.PerasCert{Base.pcRoundNo, Base.pcBoostedBlock, Base.pcVoters, Base.pcSignature} -> do
      let voters' = fromPerasCertVoters pcVoters
      case Map.keys (NEMap.filter isJust voters') of
        [] ->
          pure $
            EveryoneVotesCert
              pcRoundNo
              pcBoostedBlock
              (NEMap.keysSet voters')
              (toAggregateSignature pcSignature)
        nonPersistentSeats ->
          Left $
            EveryoneVotesButFoundNonPersistentVotersInCert
              (NonEmpty.fromList nonPersistentSeats)
