module System.Posix.Recursive (
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
import System.Posix.Files (FileStatus)

import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix

listAll' :: Bool -> (FilePath -> Bool) -> ([FilePath] -> [FilePath]) -> [FilePath] -> IO [FilePath]
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
        go :: ([FilePath] -> [FilePath]) -> [FilePath] -> IO ([FilePath], [FilePath] -> [FilePath])
        go acc current = do
            e <- Posix.readDirStream dirp
            if null e
                then pure (current, acc)
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in if predicate fullPath
                                    then go (acc . (fullPath :)) (fullPath : current)
                                    else go acc current
                        else go acc current


listAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listAll pred path =
    listAll' False pred (path :) [path]
{-# INLINE listAll #-}


followListAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
followListAll pred path =
    listAll' True pred (path :) [path]
{-# INLINE followListAll #-}


listEverything :: FilePath -> IO [FilePath]
listEverything =
    listAll (const True)
{-# INLINE listEverything #-}


followListEverything :: FilePath -> IO [FilePath]
followListEverything =
    followListAll (const True)
{-# INLINE followListEverything #-}


data Conf = Conf
    { preCheck :: !(FilePath -> Bool)
    , postCheck :: !(FileStatus -> FilePath -> Bool)
    , followSymlinks :: !Bool
    }


defConf :: Conf
defConf =
    Conf
        { preCheck = const True
        , postCheck = \_ _ -> True
        , followSymlinks = False
        }


listAccessible' :: Conf -> ([FilePath] -> [FilePath]) -> [FilePath] -> IO [FilePath]
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
        go :: [FilePath] -> IO [FilePath]
        go acc = do
            e <- Posix.readDirStream dirp
            if null e
                then pure acc
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in if preCheck fullPath
                                    then go (fullPath : acc)
                                    else go acc
                        else go acc


listAccessible :: Conf -> FilePath -> IO [FilePath]
listAccessible conf path =
    listAccessible' conf id [path]
{-# INLINE listAccessible #-}


listEverythingAccessible :: FilePath -> IO [FilePath]
listEverythingAccessible =
    listAccessible defConf
{-# INLINE listEverythingAccessible #-}


listDirectories :: FilePath -> IO [FilePath]
listDirectories =
    listAccessible defConf{postCheck = \file _ -> Posix.isDirectory file}
{-# INLINE listDirectories #-}


listRegularFiles :: FilePath -> IO [FilePath]
listRegularFiles =
    listAccessible defConf{postCheck = \file _ -> Posix.isRegularFile file}
{-# INLINE listRegularFiles #-}


listSymbolicLinks :: FilePath -> IO [FilePath]
listSymbolicLinks =
    listAccessible defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
{-# INLINE listSymbolicLinks #-}
