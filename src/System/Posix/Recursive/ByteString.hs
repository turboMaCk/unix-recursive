{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Posix.Recursive.ByteString (
    Conf (..),
    defConf,
    listAll,
    followListAll,
    listEverything,
    followListEverything,
    listAccessible,
    listDirectories,
    listRegularFiles,
    listEverythingAccessible,
    listSymbolicLinks,
) where

import Control.Exception (bracket, handle)
import System.IO.Error (IOError)
import System.Posix.ByteString.FilePath (RawFilePath)
import System.Posix.Files.ByteString (FileStatus)

import qualified Data.ByteString as BS
import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix


listAll' :: Bool -> (RawFilePath -> Bool) -> ([RawFilePath] -> [RawFilePath]) -> [RawFilePath] -> IO [RawFilePath]
listAll' _ _ acc [] = pure $ acc []
listAll' followSymlinks predicate acc (path : rest) =
    handle (\(_ :: IOError) -> listAll' followSymlinks predicate acc rest) $ do
        file <- getFileStatus path
        if Posix.isDirectory file
            then do
                (newRest, newAcc) <-
                    bracket
                        (Posix.openDirStream path)
                        Posix.closeDirStream
                        start
                listAll' followSymlinks predicate newAcc newRest
            else listAll' followSymlinks predicate acc rest
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus

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
    listAll' False pred (path :) [path]
{-# INLINE listAll #-}


followListAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
followListAll pred path =
    listAll' True pred (path :) [path]
{-# INLINE followListAll #-}


listEverything :: RawFilePath -> IO [RawFilePath]
listEverything =
    listAll (const True)
{-# INLINE listEverything #-}


followListEverything :: RawFilePath -> IO [RawFilePath]
followListEverything =
    followListAll (const True)
{-# INLINE followListEverything #-}


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
        , followSymlinks = False
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


listSymbolicLinks :: RawFilePath -> IO [RawFilePath]
listSymbolicLinks =
    listAccessible defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
{-# INLINE listSymbolicLinks #-}
