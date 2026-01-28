{-# LANGUAGE OverloadedStrings #-}

-- | HTTP client for downloading ImmutableDB chunks from a CDN.
--
-- This module handles the transport layer for the Genesis Sync Accelerator.
-- It provides functions to fetch the triad of files (@.chunk@, @.primary@, @.secondary@)
-- that constitute an ImmutableDB chunk from a remote HTTP server.
module Cardano.Tools.ImmDBServer.RemoteStorage
  ( downloadChunk
  , RemoteStorageConfig (..)
  ) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (..))
import Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util (FileType (..), getFileName)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

-- | Configuration for the remote storage client.
data RemoteStorageConfig = RemoteStorageConfig
  { rscBaseUrl :: String
  -- ^ The root URL of the CDN (e.g., "https://cdn.cardano.org/mainnet/immutable").
  , rscTargetDir :: FilePath
  -- ^ Local directory where the downloaded chunks should be stored.
  }

-- | Downloads all files associated with a specific chunk index.
--
-- This function fetches the @.chunk@, @.primary@, and @.secondary@ files.
-- If a file already exists locally, the download is skipped.
downloadChunk :: RemoteStorageConfig -> ChunkNo -> IO ()
downloadChunk cfg chunk = do
  manager <- newManager tlsManagerSettings
  let fileTypes = [ChunkFile, PrimaryIndexFile, SecondaryIndexFile]
  createDirectoryIfMissing True (rscTargetDir cfg)

  mapM_ (downloadFile manager cfg chunk) fileTypes

-- | Internal helper to download a single file using the provided HTTP 'Manager'.
downloadFile :: Manager -> RemoteStorageConfig -> ChunkNo -> FileType -> IO ()
downloadFile manager cfg chunk fileType = do
  let filename = Text.unpack $ getFileName fileType chunk
      localPath = rscTargetDir cfg </> filename
  exists <- doesFileExist localPath
  unless exists $ do
    -- Construct request
    request <- parseRequest (rscBaseUrl cfg ++ "/" ++ filename)

    -- Perform the download
    -- TODO: Add retries and progress logging.
    response <- httpLbs request manager

    let status = statusCode (responseStatus response)
    if status == 200
      then LBS.writeFile localPath (responseBody response)
      else putStrLn $ "Failed to download " ++ filename ++ ": " ++ show status
