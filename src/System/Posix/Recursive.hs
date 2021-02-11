{-# LANGUAGE OverloadedStrings #-}

module System.Posix.Recursive (list) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix


list' :: ([RawFilePath] -> [RawFilePath]) -> [RawFilePath] -> IO [RawFilePath]
list' acc [] = pure $ acc []
list' acc (path : rest) = do
    file <- Posix.getFileStatus path
    if Posix.isDirectory file
        then do
            (newRest, newAcc) <-
                bracket
                    (Posix.openDirStream path)
                    Posix.closeDirStream
                    start
            list' newAcc newRest
        else list' acc rest
  where
    {-# INLINE start #-}
    start dirp = go acc rest
      where
        go :: ([RawFilePath] -> [RawFilePath]) -> [RawFilePath] -> IO ([RawFilePath], [RawFilePath] -> [RawFilePath])
        go acc current = do
            e <- Posix.readDirStream dirp
            if BS.null e
                then pure (current, acc)
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in go (acc . (fullPath :)) (fullPath : current)
                        else go acc current


list :: RawFilePath -> IO [RawFilePath]
list path =
    list' id [path]
{-# INLINE list #-}
