{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Tools.ImmDBServer.LightweightIterator
  ( mkLightweightIterator
  ) where

import           Control.Monad (forM)
import qualified Data.ByteString.Lazy as LBS
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           Text.Printf (printf)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB.API as API hiding (tipToRealPoint)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Layout as ChunkLayout
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary (Entry (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (WithBlockSize (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Iterator (extractBlockComponent)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (fsPathChunkFile)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.IOLike
import           System.FS.API

-- | A simple iterator that reads blocks directly from a list of chunk files.
-- This bypasses the main ImmutableDB's in-memory index.
mkLightweightIterator ::
     forall m blk b h.
     ( IOLike m
     , HasHeader blk
     , DecodeDisk blk (LBS.ByteString -> blk)
     , DecodeDiskDep (NestedCtxt Header) blk
     , ReconstructNestedCtxt Header blk
     , ConvertRawHash blk
     , StandardHash blk
     , Typeable blk
     )
  => HasFS m h
  -> ChunkInfo
  -> CodecConfig blk
  -> (blk -> Bool) -- ^ Integrity check
  -> BlockComponent blk b
  -> [ChunkNo]
  -> m (Iterator m blk b)
mkLightweightIterator hasFS chunkInfo codecConfig checkIntegrity component chunks = do
    -- 1. Read all entries from all requested chunks
    allEntries <- forM chunks $ \chunk -> do
        chunkSize <- withFile hasFS (fsPathChunkFile chunk) ReadMode (hGetSize hasFS)
        -- We assume the first block might be an EBB if the chunk supports it.
        let firstIsEBB = if chunkInfoSupportsEBBs chunkInfo then IsEBB else IsNotEBB
        entries <- Secondary.readAllEntries hasFS 0 chunk (const False) chunkSize firstIsEBB
        return $ map (chunk,) entries
    
    let flatEntries = concat allEntries
    varEntries <- newTVarIO flatEntries
    
    let next = atomically (readTVar varEntries) >>= \case
            [] -> return IteratorExhausted
            ((chunk, WithBlockSize size entry) : rest) -> do
                atomically $ writeTVar varEntries rest
                res <- withFile hasFS (fsPathChunkFile chunk) ReadMode $ \hnd ->
                    extractBlockComponent hasFS chunkInfo chunk codecConfig checkIntegrity hnd (WithBlockSize size entry) component
                return $ IteratorResult res

        hasNext = readTVar varEntries >>= \case
            [] -> return Nothing
            ((_, WithBlockSize _ entry) : _) -> 
                return $ Just (tipToRealPoint chunkInfo entry)

        close = return ()

    return Iterator { iteratorNext = next, iteratorHasNext = hasNext, iteratorClose = close }

-- Helper to convert Entry to RealPoint
tipToRealPoint :: ChunkInfo -> Entry blk -> RealPoint blk
tipToRealPoint ci Secondary.Entry{blockOrEBB, Secondary.headerHash} =
    RealPoint (ChunkLayout.slotNoOfBlockOrEBB ci blockOrEBB) headerHash
