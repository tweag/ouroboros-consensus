{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | A lightweight iterator for reading ImmutableDB chunks directly from disk.
--
-- This module provides an implementation of the 'Iterator' interface that bypasses
-- the standard 'ImmutableDB' in-memory index. It is designed for the Genesis Sync
-- Accelerator to serve blocks from newly downloaded chunk files without requiring
-- a database restart or costly re-indexing.
--
-- The iterator reads the secondary index (@.secondary@) file to locate block offsets
-- and then reads the raw block data from the chunk (@.chunk@) file.
module Cardano.Tools.ImmDBServer.LightweightIterator
  ( mkLightweightIterator
  ) where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy as LBS
import Ouroboros.Consensus.Block hiding (headerHash)
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.ImmutableDB.API hiding (tipToRealPoint)
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout as ChunkLayout
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary (Entry (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator (extractBlockComponent)
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (WithBlockSize (..))
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (fsPathChunkFile)
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.IOLike
import System.FS.API

-- | Creates a "Lightweight Iterator" that serves blocks from a specific list of chunks.
--
-- This iterator is "stateless" in the sense that it does not rely on the global
-- 'ImmutableDB' state (tip, indices, etc.). Instead, it directly parses the
-- secondary index files on disk to find the requested blocks.
--
-- __Usage:__
-- This is intended to be used when the 'ImmutableDB' does not yet know about the
-- requested chunks (e.g., they were just downloaded from a CDN).
--
-- __Performance Note:__
-- This implementation pre-loads all index entries for the requested chunks into
-- memory upon creation. For very large ranges, this might need optimization
-- (e.g., streaming the index files).
mkLightweightIterator ::
  forall m blk b h.
  ( IOLike m
  , HasHeader blk
  , DecodeDisk blk (LBS.ByteString -> blk)
  , DecodeDiskDep (NestedCtxt Header) blk
  , ReconstructNestedCtxt Header blk
  , ConvertRawHash blk
  ) =>
  HasFS m h ->
  ChunkInfo ->
  CodecConfig blk ->
  -- | Integrity check function to validate blocks read from disk.
  (blk -> Bool) ->
  -- | The component of the block to stream (e.g., the whole block, just the header, etc.).
  BlockComponent blk b ->
  -- | The list of chunks (epochs) to iterate over.
  [ChunkNo] ->
  m (Iterator m blk b)
mkLightweightIterator hasFS chunkInfo codecConfig checkIntegrity component chunks = do
  -- 1. Read all entries from all requested chunks.
  -- We map over the chunks, open the corresponding secondary index file, and parse all entries.
  allEntries <- forM chunks $ \chunk -> do
    chunkSize <- withFile hasFS (fsPathChunkFile chunk) ReadMode (hGetSize hasFS)
    -- We assume the first block might be an EBB if the chunk supports it.
    let firstIsEBB = if chunkInfoSupportsEBBs chunkInfo then IsEBB else IsNotEBB
    entries <- Secondary.readAllEntries hasFS 0 chunk (const False) chunkSize firstIsEBB
    return $ map (chunk,) entries

  let flatEntries = concat allEntries
  varEntries <- newTVarIO flatEntries

  -- 2. Define the 'iteratorNext' action.
  -- This action pops the next entry from the queue, opens the corresponding chunk file,
  -- reads the block data, and extracts the requested component.
  let next =
        atomically (readTVar varEntries) >>= \case
          [] -> return IteratorExhausted
          ((chunk, WithBlockSize size entry) : rest) -> do
            atomically $ writeTVar varEntries rest
            -- We open the file for every block. This is inefficient but safe.
            -- Optimization: Keep the file handle open until the chunk changes.
            res <- withFile hasFS (fsPathChunkFile chunk) ReadMode $ \hnd ->
              extractBlockComponent
                hasFS
                chunkInfo
                chunk
                codecConfig
                checkIntegrity
                hnd
                (WithBlockSize size entry)
                component
            return $ IteratorResult res

      -- 3. Define the 'iteratorHasNext' action.
      -- Peeks at the next entry in the queue to return its Point.
      hasNext =
        readTVar varEntries >>= \case
          [] -> return Nothing
          ((_, WithBlockSize _ entry) : _) ->
            return $ Just (tipToRealPoint chunkInfo entry)

      -- 4. Define the 'iteratorClose' action.
      -- Since we don't keep persistent file handles (we open/close per block),
      -- there is nothing to clean up here.
      close = return ()

  return Iterator{iteratorNext = next, iteratorHasNext = hasNext, iteratorClose = close}

-- | Helper to convert an Index 'Entry' (which stores hash and slot/epoch)
-- into a 'RealPoint' (which uses SlotNo).
tipToRealPoint :: ChunkInfo -> Entry blk -> RealPoint blk
tipToRealPoint ci Secondary.Entry{blockOrEBB, headerHash} =
  RealPoint (ChunkLayout.slotNoOfBlockOrEBB ci blockOrEBB) headerHash
