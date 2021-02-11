{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Posix.Recursive (
    Conf (..),
    defConf,
    listAll,
    listEverything,
    listAccessible,
    listDirectories,
    listRegularFiles,
    listEverythingAccessible,
) where

import Control.Exception (bracket, handle)
import System.IO.Error (IOError)
import System.Posix.ByteString.FilePath (RawFilePath)
import System.Posix.Files.ByteString (FileStatus)

import qualified Data.ByteString as BS
import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix


listAll' :: (RawFilePath -> Bool) -> ([RawFilePath] -> [RawFilePath]) -> [RawFilePath] -> IO [RawFilePath]
listAll' _ acc [] = pure $ acc []
listAll' predicate acc (path : rest) =
    handle (\(_ :: IOError) -> listAll' predicate acc rest) $ do
        file <- Posix.getFileStatus path
        if Posix.isDirectory file
            then do
                (newRest, newAcc) <-
                    bracket
                        (Posix.openDirStream path)
                        Posix.closeDirStream
                        start
                listAll' predicate newAcc newRest
            else listAll' predicate acc rest
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
                             in if predicate fullPath
                                    then go (acc . (fullPath :)) (fullPath : current)
                                    else go acc current
                        else go acc current


listAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listAll pred path =
    listAll' pred (path :) [path]
{-# INLINE listAll #-}


listEverything :: RawFilePath -> IO [RawFilePath]
listEverything =
    listAll (const True)
{-# INLINE listEverything #-}


data Conf = Conf
    { preCheck :: !(RawFilePath -> Bool)
    , postCheck :: !(FileStatus -> RawFilePath -> Bool)
    , followSymlinks :: !Bool
    }


defConf :: Conf
defConf =
    Conf
        { preCheck = const True
        , postCheck = \_ _ -> True
        , followSymlinks = True
        }


listAccessible' :: Conf -> ([RawFilePath] -> [RawFilePath]) -> [RawFilePath] -> IO [RawFilePath]
listAccessible' _ acc [] = pure $ acc []
listAccessible' Conf{..} acc (path : rest) =
    handle (\(_ :: IOError) -> listAccessible' Conf{..} acc rest) $ do
        file <- getFileStatus path
        let newAcc =
                if postCheck file path
                    then acc . (path :)
                    else acc
        if Posix.isDirectory file
            then do
                newRest <-
                    bracket
                        (Posix.openDirStream path)
                        Posix.closeDirStream
                        start
                listAccessible' Conf{..} newAcc newRest
            else listAccessible' Conf{..} newAcc rest
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus

    {-# INLINE start #-}
    start dirp = go rest
      where
        go :: [RawFilePath] -> IO [RawFilePath]
        go acc = do
            e <- Posix.readDirStream dirp
            if BS.null e
                then pure acc
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in if preCheck fullPath
                                    then go (fullPath : acc)
                                    else go acc
                        else go acc


listAccessible :: Conf -> RawFilePath -> IO [RawFilePath]
listAccessible conf path =
    listAccessible' conf id [path]
{-# INLINE listAccessible #-}


listEverythingAccessible :: RawFilePath -> IO [RawFilePath]
listEverythingAccessible =
    listAccessible defConf
{-# INLINE listEverythingAccessible #-}


listDirectories :: RawFilePath -> IO [RawFilePath]
listDirectories =
    listAccessible defConf{postCheck = \file _ -> Posix.isDirectory file}
{-# INLINE listDirectories #-}


listRegularFiles :: RawFilePath -> IO [RawFilePath]
listRegularFiles =
    listAccessible defConf{postCheck = \file _ -> Posix.isRegularFile file}
{-# INLINE listRegularFiles #-}
