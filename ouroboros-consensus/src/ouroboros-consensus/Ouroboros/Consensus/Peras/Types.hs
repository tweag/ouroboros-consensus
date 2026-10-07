{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Base Peras types used throughout the codebase.
module Ouroboros.Consensus.Peras.Types
  ( -- * Peras round numbers
    PerasRoundNo (..)
  , onPerasRoundNo
  , zeroPerasRoundNo

    -- * Peras boosted blocks
  , PerasBoostedBlock (..)
  , BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)

    -- * Peras seat indices
  , PerasSeatIndex (..)

    -- * Peras vote parameters
  , PerasCertSize (..)
  , PerasVoteTarget (..)
  , PerasVoteId (..)
  , VoteWeight (..) -- Re-exported from Committee.Types for convenience
  )
where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeBytes
  , decodeListLenOf
  , encodeBytes
  , encodeListLen
  )
import Codec.Serialise.Class (Serialise (..))
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Coerce (coerce)
import Cardano.Crypto.Peras
  ( PerasBlockRef (..)
  , PerasBoostedBlock (..)
  , PerasRoundNo (..)
  , PerasSeatIndex (..)
  , PerasSignature (..)
  , PerasVRFOutput (..)
  , mkPerasBlockRef
  , onPerasRoundNo
  , perasBlockHashSize
  )
import Cardano.Crypto.Peras.Cert (PerasCertSize (..))
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract
  ( ConvertRawHash (..)
  , Point
  )
import Ouroboros.Consensus.Block.RealPoint
  ( fromPerasBlockRef
  , pointToWithOriginRealPoint
  , toPerasBlockRef
  , withOriginRealPointToPoint
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Committee.Types (VoteWeight (..))
import Ouroboros.Consensus.Util (ShowProxy (..))
import Ouroboros.Consensus.Util.CBOR (decodeWithOrigin, encodeWithOrigin)
import Ouroboros.Consensus.Util.Condense (Condense (..))

-- * Peras Round numbers

instance ToCBOR PerasRoundNo where
  toCBOR = toCBOR . unPerasRoundNo

instance FromCBOR PerasRoundNo where
  fromCBOR = PerasRoundNo <$> fromCBOR

zeroPerasRoundNo :: PerasRoundNo
zeroPerasRoundNo = PerasRoundNo 0

instance Condense PerasRoundNo where
  condense = show . unPerasRoundNo

instance ShowProxy PerasRoundNo where
  showProxy _ = "PerasRoundNo"

-- * Peras seat indices

instance ToCBOR PerasSeatIndex where
  toCBOR = toCBOR . unPerasSeatIndex

instance FromCBOR PerasSeatIndex where
  fromCBOR = PerasSeatIndex <$> fromCBOR

-- * Peras block references and boosted blocks

instance ToCBOR PerasBlockRef where
  toCBOR (PerasBlockRef s h) =
    mconcat
      [ encodeListLen 2
      , toCBOR s
      , encodeBytes (SBS.fromShort h)
      ]

instance FromCBOR PerasBlockRef where
  fromCBOR = do
    decodeListLenOf 2
    s <- fromCBOR
    h <- SBS.toShort <$> decodeBytes
    case mkPerasBlockRef s h of
      Just ref -> pure ref
      Nothing ->
        fail $
          "PerasBlockRef: expected a hash of "
            <> show perasBlockHashSize
            <> " bytes, got "
            <> show (SBS.length h)

instance ToCBOR PerasBoostedBlock where
  toCBOR = encodeWithOrigin toCBOR . unPerasBoostedBlock

instance FromCBOR PerasBoostedBlock where
  fromCBOR = PerasBoostedBlock <$> decodeWithOrigin fromCBOR

-- * Peras signatures and VRF outputs

instance ToCBOR PerasSignature where
  toCBOR = toCBOR . unPerasSignature

instance FromCBOR PerasSignature where
  fromCBOR = PerasSignature <$> fromCBOR

instance ToCBOR PerasVRFOutput where
  toCBOR = toCBOR . unPerasVRFOutput

instance FromCBOR PerasVRFOutput where
  fromCBOR = PerasVRFOutput <$> fromCBOR

-- ** Peras boosted blocks

-- | Conversion back and forth between boosted blocks and abstract points.
class BoostedBlockCompatibleWithPoint boostedBlock blk where
  boostedBlockToPoint :: boostedBlock -> Point blk
  pointToBoostedBlock :: Point blk -> boostedBlock

type family BoostedBlock voteOrCert :: Type

instance BoostedBlockCompatibleWithPoint (Point blk) blk where
  boostedBlockToPoint = id
  pointToBoostedBlock = id

type instance BoostedBlock (WithArrivalTime voteOrCert) = BoostedBlock voteOrCert

instance
  ( ConvertRawHash blk
  , HashSize blk ~ 32
  ) =>
  BoostedBlockCompatibleWithPoint PerasBoostedBlock blk
  where
  boostedBlockToPoint =
    withOriginRealPointToPoint
      . fmap fromPerasBlockRef
      . unPerasBoostedBlock
  pointToBoostedBlock =
    PerasBoostedBlock
      . fmap toPerasBlockRef
      . pointToWithOriginRealPoint

-- * Peras vote parameters

-- | The target of a vote in a Peras election
data PerasVoteTarget blk
  = PerasVoteTarget
  { pvtRoundNo :: !PerasRoundNo
  , pvtBlock :: !(Point blk)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | The identifier of a vote in a Peras election
data PerasVoteId
  = PerasVoteId
  { pviRoundNo :: !PerasRoundNo
  , pviSeatIndex :: !PerasSeatIndex
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance ShowProxy PerasVoteId where
  showProxy _ = "PerasVoteId"

instance FromCBOR PerasVoteId where
  fromCBOR = do
    decodeListLenOf 2
    pviRoundNo <- fromCBOR
    pviSeatIndex <- fromCBOR
    pure
      PerasVoteId
        { pviRoundNo
        , pviSeatIndex
        }

instance ToCBOR PerasVoteId where
  toCBOR
    PerasVoteId
      { pviRoundNo
      , pviSeatIndex
      } =
      encodeListLen 2
        <> toCBOR pviRoundNo
        <> toCBOR pviSeatIndex
