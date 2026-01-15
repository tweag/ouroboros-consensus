{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tools.ImmDBServer.RemoteStorage
  ( downloadChunk
  , RemoteStorageConfig (..)
  ) where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

data RemoteStorageConfig = RemoteStorageConfig
  { rscBaseUrl :: String
  , rscTargetDir :: FilePath
  }

-- | Download all files associated with a chunk (.chunk, .primary, .secondary)
downloadChunk :: RemoteStorageConfig -> ChunkNo -> IO ()
downloadChunk cfg chunk = do
  manager <- newManager tlsManagerSettings
  let extensions = ["chunk", "primary", "secondary"]
  createDirectoryIfMissing True (rscTargetDir cfg)

  mapM_ (downloadFile manager cfg chunk) extensions

formatChunkFile :: ChunkNo -> String -> String
formatChunkFile (ChunkNo n) ext = printf "%05d.%s" n ext

downloadFile :: Manager -> RemoteStorageConfig -> ChunkNo -> String -> IO ()
downloadFile manager cfg chunk ext = do
  let filename = formatChunkFile chunk ext
      localPath = rscTargetDir cfg </> filename
  exists <- doesFileExist localPath
  unless exists $ do
    request <- parseRequest (rscBaseUrl cfg ++ "/" ++ filename)
    response <- httpLbs request manager
    let status = statusCode (responseStatus response)
    if status == 200
      then LBS.writeFile localPath (responseBody response)
      else putStrLn $ "Failed to download " ++ filename ++ ": " ++ show status
