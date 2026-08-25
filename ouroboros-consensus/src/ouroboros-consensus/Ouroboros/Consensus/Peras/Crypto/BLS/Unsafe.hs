{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Temporary hack for retrieving BLS keys from the environment.
--
-- * Our private key can be read directly from the value of the environment
-- variable 'PERAS_PRIVATE_KEY'.
--
-- NOTE: keys read using this module always have the "TESTNET" scope.
--
-- WARNING: this is a temporary hack for testing purposes, and should not be
-- used under any circumstances in production. This will be replaced with proper
-- on-chain key registration in the future.
module Ouroboros.Consensus.Peras.Crypto.BLS.Unsafe
  ( unsafePerasBLSPrivateKeyFromEnv
  ) where

import qualified Data.ByteString.Base16 as B16
import Data.String (IsString (..))
import qualified Ouroboros.Consensus.Committee.Crypto.BLS as BLS
import Ouroboros.Consensus.Peras.Crypto.BLS (PerasPrivateKey (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

keyScope :: BLS.KeyScope
keyScope = "TESTNET"

-- | Read a private key from the environment variable 'PERAS_PRIVATE_KEY'
unsafePerasBLSPrivateKeyFromEnv :: Either String PerasPrivateKey
unsafePerasBLSPrivateKeyFromEnv =
  unsafePerformIO $
    lookupEnv envVar >>= \case
      Nothing ->
        pure $ Left $ "Environment variable " <> envVar <> "not set."
      Just rawKey ->
        pure $ decodeKey rawKey
 where
  envVar =
    "PERAS_PRIVATE_KEY"

  decodeKey key =
    case B16.decode (fromString key) of
      Left msg ->
        Left $ "Invalid private key format: " <> msg
      Right keyBytes -> do
        case BLS.rawDeserialisePrivateKey keyScope keyBytes of
          Nothing -> do
            Left $ "Invalid private key format: " <> key
          Just sk -> do
            Right $ PerasPrivateKey sk
{-# NOINLINE unsafePerasBLSPrivateKeyFromEnv #-}
