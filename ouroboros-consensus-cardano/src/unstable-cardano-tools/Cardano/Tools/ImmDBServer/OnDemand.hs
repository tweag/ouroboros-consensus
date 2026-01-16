{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | On-demand fetching wrapper for ImmutableDB.
--
-- This module provides a decorator for 'ImmutableDB' that intercepts streaming
-- requests. If the requested data is beyond the current indexed tip, it downloads
-- the missing chunks from a CDN and serves them using a 'LightweightIterator'.
module Cardano.Tools.ImmDBServer.OnDemand
  ( decorateImmutableDB
  , OnDemandConfig (..)
  ) where

import qualified Cardano.Tools.ImmDBServer.LightweightIterator as Lightweight
import qualified Cardano.Tools.ImmDBServer.RemoteStorage as Remote
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
  ( CodecConfig
  , ConvertRawHash
  , HasHeader
  , Header
  , NestedCtxt
  , WithOrigin (..)
  , pointSlot
  , realPointSlot
  )
import Ouroboros.Consensus.Block.RealPoint (realPointToPoint)
import Ouroboros.Consensus.Storage.Common (StreamFrom (..), StreamTo (..))
import Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB (..), getTipPoint)
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkInfo, ChunkNo (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal as ChunkInfo
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout as ChunkLayout
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
  ( fsPathChunkFile
  , fsPathPrimaryIndexFile
  , fsPathSecondaryIndexFile
  )
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks
  , SomeException
  , StrictTVar
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , try
  )
import Ouroboros.Consensus.Util.NormalForm.StrictTVar (writeTVar)
import System.FS.API (HasFS, removeFile)

-- | Configuration for the On-Demand decorator.
data OnDemandConfig m blk h = OnDemandConfig
  { odcRemote :: Remote.RemoteStorageConfig
  -- ^ CDN connection details.
  , odcChunkInfo :: ChunkInfo
  -- ^ Information about chunk sizes used for slot-to-chunk translation.
  , odcHasFS :: HasFS m h
  -- ^ File system handle for saving downloaded chunks.
  , odcCodecConfig :: CodecConfig blk
  -- ^ Codec configuration for block extraction.
  , odcCheckIntegrity :: blk -> Bool
  -- ^ Integrity check for extracted blocks.
  , odcMaxCachedChunks :: Int
  -- ^ Maximum number of chunks to keep in cache.
  }

-- | Internal state tracking which chunks have been downloaded during the current session.
data OnDemandState = OnDemandState
  { odsCachedChunks :: Set ChunkNo
  -- ^ Set of chunk indices already present on disk.
  , odsUsageOrder :: [ChunkNo]
  -- ^ Ordered list of chunks in cache, from Most Recently Used to Least Recently Used.
  }
  deriving (Generic, NoThunks)

-- | Wraps an existing 'ImmutableDB' with on-demand fetching logic.
--
-- The resulting database will behave exactly like the original, except that
-- 'stream_' calls reaching beyond the current local tip will trigger HTTP
-- downloads of the required chunks.
decorateImmutableDB ::
  forall m blk h.
  ( IOLike m
  , MonadIO m
  , HasHeader blk
  , DecodeDisk blk (ByteString -> blk)
  , DecodeDiskDep (NestedCtxt Header) blk
  , ReconstructNestedCtxt Header blk
  , ConvertRawHash blk
  ) =>
  OnDemandConfig m blk h ->
  ImmutableDB m blk ->
  m (ImmutableDB m blk)
decorateImmutableDB cfg@OnDemandConfig{odcChunkInfo, odcHasFS, odcCodecConfig, odcCheckIntegrity} db = do
  stateVar <- newTVarIO (OnDemandState Set.empty [])
  pure $
    db
      { stream_ = \registry component from to -> do
          let requestedChunks = getChunksInRange odcChunkInfo from to

          -- Check if ImmutableDB already has this range
          tipPoint <- atomically $ getTipPoint db

          let StreamToInclusive rp = to
          let toPoint = realPointToPoint rp

          -- Logic: If we are syncing beyond the current tip, use Lightweight Reader
          if tipPoint >= toPoint
            then stream_ db registry component from to
            else do
              ensureChunks cfg stateVar requestedChunks
              Right
                <$> Lightweight.mkLightweightIterator
                  odcHasFS
                  odcChunkInfo
                  odcCodecConfig
                  odcCheckIntegrity
                  component
                  requestedChunks
      }

-- | Ensures that the requested chunks are present on disk, downloading them if necessary.
-- Implements an LRU eviction policy to maintain the 'odcMaxCachedChunks' limit.
ensureChunks ::
  forall m blk h.
  (IOLike m, MonadIO m) =>
  OnDemandConfig m blk h ->
  StrictTVar m OnDemandState ->
  [ChunkNo] ->
  m ()
ensureChunks OnDemandConfig{odcRemote, odcHasFS, odcMaxCachedChunks} stateVar requestedChunks = do
  -- 1. Identify and download missing chunks
  state <- readTVarIO stateVar
  let missingChunks = filter (\c -> not (Set.member c (odsCachedChunks state))) requestedChunks

  unless (null missingChunks) $ do
    liftIO $ mapM_ (Remote.downloadChunk odcRemote) missingChunks

  -- 2. Update usage order and identify chunks to prune
  toPrune <- atomically $ do
    curr <- readTVar stateVar
    let
      -- Move requested chunks to the head (most recently used)
      newUsage = requestedChunks ++ foldr delete (odsUsageOrder curr) requestedChunks
      newCached = Set.union (odsCachedChunks curr) (Set.fromList requestedChunks)

      -- Eviction: if we exceed the limit, prune the least recently used chunks.
      -- We must keep all chunks currently requested to ensure iterator safety.
      (stay, prune) = splitAt (max odcMaxCachedChunks (length requestedChunks)) newUsage
      updatedCached = Set.difference newCached (Set.fromList prune)

    writeTVar stateVar (OnDemandState updatedCached stay)
    return prune

  -- 3. Physically delete pruned chunks from disk
  unless (null toPrune) $
    mapM_ (deleteChunkFiles odcHasFS) toPrune

-- | Deletes the triad of files associated with a chunk.
deleteChunkFiles :: IOLike m => HasFS m h -> ChunkNo -> m ()
deleteChunkFiles hasFS chunk = do
  hRemove hasFS (fsPathChunkFile chunk)
  hRemove hasFS (fsPathPrimaryIndexFile chunk)
  hRemove hasFS (fsPathSecondaryIndexFile chunk)
 where
  hRemove h f = void $ try @_ @SomeException $ removeFile h f

-- | Identifies the set of chunks covering a given streaming range.
getChunksInRange :: ChunkInfo -> StreamFrom blk -> StreamTo blk -> [ChunkNo]
getChunksInRange chunkInfo from to =
  let startChunk = chunkForFrom chunkInfo from
      endChunk = chunkForTo chunkInfo to
   in ChunkInfo.chunksBetween startChunk endChunk

-- | Translates a 'StreamFrom' bound to its starting 'ChunkNo'.
chunkForFrom :: ChunkInfo -> StreamFrom blk -> ChunkNo
chunkForFrom ci (StreamFromInclusive pt) = ChunkLayout.chunkIndexOfSlot ci (realPointSlot pt)
chunkForFrom ci (StreamFromExclusive pt) = case pointSlot pt of
  Origin -> ChunkNo 0
  NotOrigin slot -> ChunkLayout.chunkIndexOfSlot ci slot

-- | Translates a 'StreamTo' bound to its ending 'ChunkNo'.
chunkForTo :: ChunkInfo -> StreamTo blk -> ChunkNo
chunkForTo ci (StreamToInclusive pt) = ChunkLayout.chunkIndexOfSlot ci (realPointSlot pt)
