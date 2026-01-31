{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Immutable DB utility tests.
module Test.Ouroboros.Storage.ImmutableDB.Util (tests) where

import Data.Maybe (isJust, isNothing)
import qualified Data.Text as Text
import System.FS.API.Types (FsPath, fsPathToList)

import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
  ( ChunkNo (..)
  )
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
  ( FileType (..)
  , fromSuffix
  , fsPathChunkFile
  , fsPathPrimaryIndexFile
  , fsPathSecondaryIndexFile
  , getFileName
  , parseDBFileName
  , toSuffix
  )
import Test.QuickCheck

-- for Arbitrary Text
import Test.QuickCheck.Instances ()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

-- for Arbitrary ChunkNo
import Test.Util.Orphans.Arbitrary ()

instance Arbitrary FileType where
  arbitrary =
    elements
      [ ChunkFile
      , PrimaryIndexFile
      , SecondaryIndexFile
      , EpochFile
      ]

prop_FileType_roundtrips_through_suffix :: FileType -> Property
prop_FileType_roundtrips_through_suffix ft =
  fromSuffix (toSuffix ft) === Just ft

prop_only_valid_strings_parse_as_FileType :: Property
prop_only_valid_strings_parse_as_FileType =
  noShrinking $ forAll genSuffix $ \(s, p) ->
    let check = if p then isJust else isNothing
     in check $ fromSuffix s
 where
  validSuffixes = map toSuffix [ChunkFile, PrimaryIndexFile, SecondaryIndexFile, EpochFile]

  genSuffix :: Gen (Text.Text, Bool)
  genSuffix =
    frequency
      [ (3, (,True) <$> elements validSuffixes)
      , (7, (,False) <$> arbitrary `suchThat` (`notElem` validSuffixes))
      ]

prop_FileType_roundtrips_through_getFileName_via_parseDBFileName :: FileType -> ChunkNo -> Property
prop_FileType_roundtrips_through_getFileName_via_parseDBFileName ft cn =
  let fn = Text.unpack $ getFileName ft cn
   in fmap fst (parseDBFileName fn) === Just ft

buildProp_fsPath_FileType_is_correct :: FileType -> (ChunkNo -> FsPath) -> Property
buildProp_fsPath_FileType_is_correct ft getFsPath =
  forAll arbitrary $ \cn ->
    let fp = getFsPath cn
        fn = Text.unpack . last . fsPathToList $ fp
     in fmap fst (parseDBFileName fn) === Just ft

prop_fsPathChunkFile_parses_as_ChunkFile :: Property
prop_fsPathChunkFile_parses_as_ChunkFile = buildProp_fsPath_FileType_is_correct ChunkFile fsPathChunkFile

prop_fsPathPrimaryIndexFile_parses_as_PrimaryIndexFile :: Property
prop_fsPathPrimaryIndexFile_parses_as_PrimaryIndexFile = buildProp_fsPath_FileType_is_correct PrimaryIndexFile fsPathPrimaryIndexFile

prop_fsPathSecondaryIndexFile_parses_as_SecondaryIndexFile :: Property
prop_fsPathSecondaryIndexFile_parses_as_SecondaryIndexFile = buildProp_fsPath_FileType_is_correct SecondaryIndexFile fsPathSecondaryIndexFile

tests :: TestTree
tests =
  testGroup
    "ImmutableDB utilities"
    [ testProperty "FileType roundtrips through suffix" prop_FileType_roundtrips_through_suffix
    , testProperty "Only valid strings parse as FileType" prop_only_valid_strings_parse_as_FileType
    , testProperty
        "FileType roundtrips through getFileName via parseDBFileName"
        prop_FileType_roundtrips_through_getFileName_via_parseDBFileName
    , testProperty "fsPathChunkFile parses as ChunkFile" prop_fsPathChunkFile_parses_as_ChunkFile
    , testProperty
        "fsPathPrimaryIndexFile parses as PrimaryIndexFile"
        prop_fsPathPrimaryIndexFile_parses_as_PrimaryIndexFile
    , testProperty
        "fsPathSecondaryIndexFile parses as SecondaryIndexFile"
        prop_fsPathSecondaryIndexFile_parses_as_SecondaryIndexFile
    ]