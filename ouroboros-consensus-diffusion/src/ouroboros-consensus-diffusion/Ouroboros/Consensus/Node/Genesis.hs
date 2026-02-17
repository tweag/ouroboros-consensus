{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Node.Genesis (
    -- * 'GenesisConfig'
    GenesisConfig (..)
  , GenesisConfigFlags (..)
  , GDDConfig (..)
  , defaultGenesisConfigFlags
  , disableGenesisConfig
  , enableGenesisConfigDefault
  , mkGenesisConfig
    -- * NodeKernel helpers
  , GenesisNodeKernelArgs (..)
  , GDDNodeKernelArgs (..)
  , mkGenesisNodeKernelArgs
  , setGetLoEFragment
  ) where

import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Traversable (for)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (CSJConfig (..), CSJEnabledConfig (..),
                     ChainSyncLoPBucketConfig (..),
                     ChainSyncLoPBucketEnabledConfig (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
                     (HistoricityCutoff (..))
import qualified Ouroboros.Consensus.Node.GsmState as GSM
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch
                     (GenesisBlockFetchConfiguration (..))

-- | Whether to en-/disable the Genesis Density
-- Disconnector.
data GDDConfig a =
    GDDEnabled !a
  | GDDDisabled
  deriving stock (Eq, Generic, Show, Functor, Foldable, Traversable)

-- | Aggregating the various configs for Genesis-related subcomponents.
--
-- Usually, 'enableGenesisConfigDefault' or 'disableGenesisConfig' can be used.
-- See the haddocks of the types of the individual fields for details.
data GenesisConfig = GenesisConfig
  { gcBlockFetchConfig         :: !GenesisBlockFetchConfiguration
  , gcChainSyncLoPBucketConfig :: !ChainSyncLoPBucketConfig
  , gcCSJConfig                :: !CSJConfig
  , gcGDDConfig                :: !(GDDConfig GDDParams)
  , gcHistoricityCutoff        :: !(Maybe HistoricityCutoff)
  } deriving stock (Eq, Generic, Show)

-- | Genesis configuration flags and low-level args, as parsed from config file or CLI
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ             :: Bool
  , gcfEnableGDD             :: Bool
  , gcfEnableLoP             :: Bool
  , gcfBlockFetchGracePeriod :: Maybe DiffTime
  , gcfBucketCapacity        :: Maybe Integer
  , gcfBucketRate            :: Maybe Integer
  , gcfCSJJumpSize           :: Maybe SlotNo
  , gcfGDDRateLimit          :: Maybe DiffTime
  } deriving stock (Eq, Generic, Show)

defaultGenesisConfigFlags :: GenesisConfigFlags
defaultGenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ              = True
  , gcfEnableGDD              = True
  , gcfEnableLoP              = True
  , gcfBlockFetchGracePeriod  = Nothing
  , gcfBucketCapacity         = Nothing
  , gcfBucketRate             = Nothing
  , gcfCSJJumpSize            = Nothing
  , gcfGDDRateLimit           = Nothing
  }

enableGenesisConfigDefault :: GenesisConfig
enableGenesisConfigDefault = mkGenesisConfig $ Just defaultGenesisConfigFlags

-- | Disable all Genesis components, yielding Praos behavior.
disableGenesisConfig :: GenesisConfig
disableGenesisConfig = mkGenesisConfig Nothing

mkGenesisConfig :: Maybe GenesisConfigFlags -> GenesisConfig
mkGenesisConfig Nothing = -- disable Genesis
  GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcGracePeriod = 0 -- no grace period when Genesis is disabled
        }
    , gcChainSyncLoPBucketConfig = ChainSyncLoPBucketDisabled
    , gcCSJConfig                = CSJDisabled
    , gcGDDConfig                = GDDDisabled
    , gcHistoricityCutoff        = Nothing
    }
mkGenesisConfig (Just cfg) =
  GenesisConfig
    { gcBlockFetchConfig = GenesisBlockFetchConfiguration
        { gbfcGracePeriod
        }
    , gcChainSyncLoPBucketConfig = if gcfEnableLoP
        then ChainSyncLoPBucketEnabled ChainSyncLoPBucketEnabledConfig
          { csbcCapacity
          , csbcRate
          }
        else ChainSyncLoPBucketDisabled
    , gcCSJConfig = if gcfEnableCSJ
        then CSJEnabled CSJEnabledConfig
          { csjcJumpSize
          }
        else CSJDisabled
    , gcGDDConfig = if gcfEnableGDD
        then GDDEnabled GDDParams{lgpGDDRateLimit}
        else GDDDisabled
    , -- Duration in seconds of one Cardano mainnet Shelley stability window
      -- (3k/f slots times one second per slot) plus one extra hour as a
      -- safety margin.
      gcHistoricityCutoff = Just $ HistoricityCutoff $ 3 * 2160 * 20 + 3600
    }
  where
    GenesisConfigFlags {
        gcfEnableLoP
      , gcfEnableCSJ
      , gcfEnableGDD
      , gcfBlockFetchGracePeriod
      , gcfBucketCapacity
      , gcfBucketRate
      , gcfCSJJumpSize
      , gcfGDDRateLimit
      } = cfg

    -- The minimum amount of time during which the Genesis BlockFetch logic will
    -- download blocks from a specific peer (even if it is not performing well
    -- during that period).
    defaultBlockFetchGracePeriod = 10 -- seconds

    -- LoP parameters. Empirically, it takes less than 1ms to validate a header,
    -- so leaking one token per 2ms is conservative. The capacity of 100_000
    -- tokens corresponds to 200s, which is definitely enough to handle long GC
    -- pauses; we could even make this more conservative.
    defaultCapacity = 100_000 -- number of tokens
    defaultRate     = 500 -- tokens per second leaking, 1/2ms

    -- The larger Shelley forecast range (3 * 2160 * 20) works in more recent
    -- ranges of slots, but causes syncing to block in Byron. A future
    -- improvement would be to make this era-dynamic, such that we can use the
    -- larger (and hence more efficient) larger CSJ jump size in Shelley-based
    -- eras.
    defaultCSJJumpSize = 2 * 2160 -- Byron forecast range

    -- Limiting the performance impact of the GDD.
    defaultGDDRateLimit        = 1.0 -- seconds

    gbfcGracePeriod = fromMaybe defaultBlockFetchGracePeriod gcfBlockFetchGracePeriod
    csbcCapacity    = fromMaybe defaultCapacity gcfBucketCapacity
    csbcRate        = maybe defaultRate (fromInteger @Rational) gcfBucketRate
    csjcJumpSize    = fromMaybe defaultCSJJumpSize gcfCSJJumpSize
    lgpGDDRateLimit = fromMaybe defaultGDDRateLimit gcfGDDRateLimit

newtype GDDParams = GDDParams
  { -- | How often to evaluate GDD. 0 means as soon as possible.
    -- Otherwise, no faster than once every T seconds, where T is the
    -- value of the field.
    lgpGDDRateLimit :: DiffTime
  } deriving stock (Eq, Generic, Show)

-- | Genesis-related arguments needed by the NodeKernel initialization logic.
data GenesisNodeKernelArgs blk = GenesisNodeKernelArgs {
    gnkaGDDArgs :: !(GDDConfig (GDDNodeKernelArgs blk))
  }

data GDDNodeKernelArgs blk = GDDNodeKernelArgs {
  lgnkaGDDRateLimit    :: DiffTime
  }

-- | Create the initial 'GenesisNodeKernelArgs" .
mkGenesisNodeKernelArgs ::
     forall m blk. (IOLike m)
  => GenesisConfig
  -> m (GenesisNodeKernelArgs blk)
mkGenesisNodeKernelArgs gcfg = do
    gnkaGDDArgs <- for (gcGDDConfig gcfg) $ \p -> do
        pure GDDNodeKernelArgs
          { lgnkaGDDRateLimit = lgpGDDRateLimit p
          }
    pure GenesisNodeKernelArgs{gnkaGDDArgs}

-- | Set the actual logic for determining the current LoE fragment.
setGetLoEFragment ::
     forall m blk. (IOLike m, GetHeader blk, Typeable blk)
  => STM m (ChainDB.LeashingState blk)
  -> STM m GSM.GsmState
  -> STM m (Maybe (AnchoredFragment (HeaderWithTime blk)))
     -- ^ The Genesis LoE fragment.
  -> STM m (Maybe (AnchoredFragment (HeaderWithTime blk)))
     -- ^ The LoE fragment.
  -> StrictTVar m (ChainDB.GetLoEFragment m blk)
  -> m ()
setGetLoEFragment readLeashingState readGsmState readGenesisLoEFragment readLoEFragment varGetLoEFragment =
    atomically $ writeTVar varGetLoEFragment getLoEFragment
  where
    getLoEFragment :: ChainDB.GetLoEFragment m blk
    getLoEFragment = atomically $ do
      leashingState <- readLeashingState
      if not $ Map.null leashingState then
        readLoEFragment >>= \case
          Just loeFrag -> pure $ ChainDB.LoEEnabled loeFrag
          Nothing -> pure ChainDB.LoEDisabled
      else 
        readGenesisLoEFragment >>= \case
          Just glf -> do
            -- leashing is disabled, run old behavior
            readGsmState >>= \case
              -- When the Honest Availability Assumption cannot currently be
              -- guaranteed, we should not select any blocks that would cause our
              -- immutable tip to advance, so we return the most conservative LoE
              -- fragment.
              GSM.PreSyncing ->
                pure $ ChainDB.LoEEnabled $ AF.Empty AF.AnchorGenesis
              -- When we are syncing, return the current LoE fragment.
              GSM.Syncing    ->
                pure $ ChainDB.LoEEnabled glf 
              -- When we are caught up, the LoE is disabled.
              GSM.CaughtUp   ->
                pure ChainDB.LoEDisabled
          Nothing -> 
            pure ChainDB.LoEDisabled

