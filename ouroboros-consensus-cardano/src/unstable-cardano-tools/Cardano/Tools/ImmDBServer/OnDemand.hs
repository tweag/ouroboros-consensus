{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.ImmDBServer.OnDemand
  ( decorateImmutableDB
  , OnDemandConfig (..)
  ) where

import qualified Cardano.Tools.ImmDBServer.LightweightIterator as Lightweight
import qualified Cardano.Tools.ImmDBServer.RemoteStorage as Remote
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy (ByteString)
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
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike
  ( IOLike
  , NoThunks
  , StrictTVar
  , atomically
  , modifyTVar
  , newTVarIO
  , readTVarIO
  )
import System.FS.API (HasFS)

-- | Config for OnDemand fetching
data OnDemandConfig m blk h = OnDemandConfig
  { odcRemote :: Remote.RemoteStorageConfig
  , odcChunkInfo :: ChunkInfo
  , odcHasFS :: HasFS m h
  , odcCodecConfig :: CodecConfig blk
  , odcCheckIntegrity :: blk -> Bool
  }

data OnDemandState = OnDemandState
  { odsCachedChunks :: Set ChunkNo
  }
  deriving (Generic, NoThunks)

decorateImmutableDB ::
  forall m blk h.
  ( IOLike m
  , MonadIO m
  , HasHeader blk
  , DecodeDisk blk (ByteString -> blk)
  , DecodeDiskDep (NestedCtxt Header) blk
  , ReconstructNestedCtxt Header blk
  , ConvertRawHash blk
  , NoThunks OnDemandState
  ) =>
  OnDemandConfig m blk h ->
  ImmutableDB m blk ->
  m (ImmutableDB m blk)
decorateImmutableDB cfg@OnDemandConfig{odcChunkInfo, odcHasFS, odcCodecConfig, odcCheckIntegrity} db = do
  stateVar <- newTVarIO (OnDemandState Set.empty)
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

ensureChunks ::
  (IOLike m, MonadIO m) =>
  OnDemandConfig m blk h ->
  StrictTVar m OnDemandState ->
  [ChunkNo] ->
  m ()
ensureChunks OnDemandConfig{odcRemote} stateVar requestedChunks = do
  state <- readTVarIO stateVar
  let missingChunks = filter (\c -> not (Set.member c (odsCachedChunks state))) requestedChunks

  unless (null missingChunks) $ do
    liftIO $ mapM_ (Remote.downloadChunk odcRemote) missingChunks
    atomically $ modifyTVar stateVar $ \s ->
      s{odsCachedChunks = Set.union (odsCachedChunks s) (Set.fromList missingChunks)}

getChunksInRange :: ChunkInfo -> StreamFrom blk -> StreamTo blk -> [ChunkNo]
getChunksInRange chunkInfo from to =
  let startChunk = chunkForFrom chunkInfo from
      endChunk = chunkForTo chunkInfo to
   in ChunkInfo.chunksBetween startChunk endChunk

chunkForFrom :: ChunkInfo -> StreamFrom blk -> ChunkNo
chunkForFrom ci (StreamFromInclusive pt) = ChunkLayout.chunkIndexOfSlot ci (realPointSlot pt)
chunkForFrom ci (StreamFromExclusive pt) = case pointSlot pt of
  Origin -> ChunkNo 0
  NotOrigin slot -> ChunkLayout.chunkIndexOfSlot ci slot

chunkForTo :: ChunkInfo -> StreamTo blk -> ChunkNo
chunkForTo ci (StreamToInclusive pt) = ChunkLayout.chunkIndexOfSlot ci (realPointSlot pt)
