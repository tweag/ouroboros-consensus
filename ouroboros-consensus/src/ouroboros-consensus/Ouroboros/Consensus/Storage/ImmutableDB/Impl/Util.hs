{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
  ( -- * Utilities
    FileType (..)
  , Two (..)
  , checkChecksum
  , dbFilesOnDisk
  , fsPathChunkFile
  , fsPathPrimaryIndexFile
  , fsPathSecondaryIndexFile
  , getFileName
  , parseDBFileName
  , partialParseDBFileName
  , removeFilesStartingFrom
  , getFsPath
  , runGet
  , runGetWithUnconsumed
  , toSuffix
  , tryImmutableDB
  , wrapFsError
  ) where

import Control.Monad (forM_)
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as Lazy
import Data.List as List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block hiding (hashSize)
import Ouroboros.Consensus.Storage.ImmutableDB.API
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
  ( ChunkNo (..)
  )
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import System.FS.API
import System.FS.CRC
import Text.Read (readMaybe)

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

-- | Useful when you have exactly two values of some type and want to
-- 'traverse' over both of them (which is not possible with a tuple).
data Two a = Two a a
  deriving (Functor, Foldable, Traversable)

data FileType = ChunkFile | PrimaryIndexFile | SecondaryIndexFile | EpochFile
  deriving (Eq, Show)

toSuffix :: FileType -> Text
toSuffix = \case
  ChunkFile -> "chunk"
  PrimaryIndexFile -> "primary"
  SecondaryIndexFile -> "secondary"
  EpochFile -> "epoch"

fromSuffix :: Text -> Maybe FileType
fromSuffix = \case
  "chunk" -> Just ChunkFile
  "primary" -> Just PrimaryIndexFile
  "secondary" -> Just SecondaryIndexFile
  "epoch" -> Just EpochFile
  _ -> Nothing

fsPathChunkFile :: ChunkNo -> FsPath
fsPathChunkFile = getFsPath ChunkFile

fsPathPrimaryIndexFile :: ChunkNo -> FsPath
fsPathPrimaryIndexFile = getFsPath PrimaryIndexFile

fsPathSecondaryIndexFile :: ChunkNo -> FsPath
fsPathSecondaryIndexFile = getFsPath SecondaryIndexFile

getFileName :: FileType -> ChunkNo -> Text
getFileName fileType (ChunkNo chunk) = T.justifyRight 5 '0' (T.pack (show chunk)) <> "." <> toSuffix fileType

-- | Opposite of 'parseDBFileName'.
getFsPath :: FileType -> ChunkNo -> FsPath
getFsPath ft cn = fsPathFromList [getFileName ft cn]

partialParseDBFileName :: String -> Maybe (Text, ChunkNo)
partialParseDBFileName s = case T.splitOn "." $ T.pack s of
  [n, ext] -> (\cn -> (ext, ChunkNo cn)) <$> readMaybe (T.unpack n)
  _ -> Nothing

-- | Parse the prefix and chunk number from the filename of an index or chunk
-- file.
--
-- > parseDBFileName "00001.chunk"
-- Just (ChunkFile, 1)
-- > parseDBFileName "00012.primary"
-- Just (PrimaryIndexFile, 12)
parseDBFileName :: String -> Maybe (FileType, ChunkNo)
parseDBFileName s = partialParseDBFileName s >>= (\(ext, cn) -> (\ft -> (ft, cn)) <$> fromSuffix ext)

-- | Go through all files, making three sets: the set of chunk files, primary
-- index files, and secondary index files, discarding all others.
dbFilesOnDisk :: Set String -> (Set ChunkNo, Set ChunkNo, Set ChunkNo)
dbFilesOnDisk = List.foldl' categorise mempty
 where
  categorise fs@(!chunk, !primary, !secondary) file =
    case parseDBFileName file of
      Just (ChunkFile, n) -> (Set.insert n chunk, primary, secondary)
      Just (PrimaryIndexFile, n) -> (chunk, Set.insert n primary, secondary)
      Just (SecondaryIndexFile, n) -> (chunk, primary, Set.insert n secondary)
      Just (EpochFile, _) -> fs
      _ -> fs

-- | Remove all chunk and index starting from the given chunk (included).
removeFilesStartingFrom ::
  (HasCallStack, Monad m) =>
  HasFS m h ->
  ChunkNo ->
  m ()
removeFilesStartingFrom HasFS{removeFile, listDirectory} chunk = do
  filesInDBFolder <- listDirectory (mkFsPath [])
  let (chunkFiles, primaryFiles, secondaryFiles) = dbFilesOnDisk filesInDBFolder
  forM_ (takeWhile (>= chunk) (Set.toDescList chunkFiles)) $ \e ->
    removeFile (fsPathChunkFile e)
  forM_ (takeWhile (>= chunk) (Set.toDescList primaryFiles)) $ \i ->
    removeFile (fsPathPrimaryIndexFile i)
  forM_ (takeWhile (>= chunk) (Set.toDescList secondaryFiles)) $ \i ->
    removeFile (fsPathSecondaryIndexFile i)

-- | Rewrap 'FsError' in a 'ImmutableDBError'.
wrapFsError ::
  forall blk m a.
  (MonadCatch m, StandardHash blk, Typeable blk) =>
  Proxy blk ->
  m a ->
  m a
wrapFsError _ = handle $ throwUnexpectedFailure @blk . FileSystemError

-- | Execute an action and catch the 'ImmutableDBError' and 'FsError' that can
-- be thrown by it, and wrap the 'FsError' in an 'ImmutableDBError' using the
-- 'FileSystemError' constructor.
--
-- This should be used whenever you want to run an action on the ImmutableDB
-- and catch the 'ImmutableDBError' and the 'FsError' (wrapped in the former)
-- it may thrown.
tryImmutableDB ::
  forall m blk a.
  (MonadCatch m, StandardHash blk, Typeable blk) =>
  Proxy blk ->
  m a ->
  m (Either (ImmutableDBError blk) a)
tryImmutableDB pb = try . wrapFsError pb

-- | Wrapper around 'Get.runGetOrFail' that throws an 'InvalidFileError' when
-- it failed or when there was unconsumed input.
runGet ::
  forall blk a m.
  (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk) =>
  Proxy blk ->
  FsPath ->
  Get a ->
  Lazy.ByteString ->
  m a
runGet _ file get bl = case Get.runGetOrFail get bl of
  Right (unconsumed, _, primary)
    | Lazy.null unconsumed ->
        return primary
    | otherwise ->
        throwUnexpectedFailure $
          InvalidFileError @blk file "left-over bytes" prettyCallStack
  Left (_, _, msg) ->
    throwUnexpectedFailure $
      InvalidFileError @blk file msg prettyCallStack

-- | Same as 'runGet', but allows unconsumed input and returns it.
runGetWithUnconsumed ::
  forall blk a m.
  (HasCallStack, MonadThrow m, StandardHash blk, Typeable blk) =>
  Proxy blk ->
  FsPath ->
  Get a ->
  Lazy.ByteString ->
  m (Lazy.ByteString, a)
runGetWithUnconsumed _ file get bl = case Get.runGetOrFail get bl of
  Right (unconsumed, _, primary) ->
    return (unconsumed, primary)
  Left (_, _, msg) ->
    throwUnexpectedFailure $
      InvalidFileError @blk file msg prettyCallStack

-- | Check whether the given checksums match. If not, throw a
-- 'ChecksumMismatchError'.
checkChecksum ::
  (HasCallStack, HasHeader blk, MonadThrow m) =>
  FsPath ->
  RealPoint blk ->
  -- | Expected checksum
  CRC ->
  -- | Actual checksum
  CRC ->
  m ()
checkChecksum chunkFile pt expected actual
  | expected == actual =
      return ()
  | otherwise =
      throwUnexpectedFailure $
        ChecksumMismatchError pt expected actual chunkFile prettyCallStack
