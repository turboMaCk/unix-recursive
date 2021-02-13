module <? echo $module_name ?> (
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

<? echo $imports ?>

listAll' :: Bool -> (<? echo $file_path_type ?> -> Bool) -> ([<? echo $file_path_type ?>] -> [<? echo $file_path_type ?>]) -> [<? echo $file_path_type ?>] -> IO [<? echo $file_path_type ?>]
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
        go :: ([<? echo $file_path_type ?>] -> [<? echo $file_path_type ?>]) -> [<? echo $file_path_type ?>] -> IO ([<? echo $file_path_type ?>], [<? echo $file_path_type ?>] -> [<? echo $file_path_type ?>])
        go acc current = do
            e <- Posix.readDirStream dirp
            if <? echo $check_empty_fc ?> e
                then pure (current, acc)
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in if predicate fullPath
                                    then go (acc . (fullPath :)) (fullPath : current)
                                    else go acc current
                        else go acc current


listAll :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAll pred path =
    listAll' False pred (path :) [path]
{-# INLINE listAll #-}


followListAll :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followListAll pred path =
    listAll' True pred (path :) [path]
{-# INLINE followListAll #-}


listEverything :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listEverything =
    listAll (const True)
{-# INLINE listEverything #-}


followListEverything :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followListEverything =
    followListAll (const True)
{-# INLINE followListEverything #-}


data Conf = Conf
    { preCheck :: !(<? echo $file_path_type ?> -> Bool)
    , postCheck :: !(FileStatus -> <? echo $file_path_type ?> -> Bool)
    , followSymlinks :: !Bool
    }


defConf :: Conf
defConf =
    Conf
        { preCheck = const True
        , postCheck = \_ _ -> True
        , followSymlinks = False
        }


listAccessible' :: Conf -> ([<? echo $file_path_type ?>] -> [<? echo $file_path_type ?>]) -> [<? echo $file_path_type ?>] -> IO [<? echo $file_path_type ?>]
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
        go :: [<? echo $file_path_type ?>] -> IO [<? echo $file_path_type ?>]
        go acc = do
            e <- Posix.readDirStream dirp
            if <? echo $check_empty_fc ?> e
                then pure acc
                else
                    if e /= "." && e /= ".."
                        then
                            let fullPath = path <> "/" <> e
                             in if preCheck fullPath
                                    then go (fullPath : acc)
                                    else go acc
                        else go acc


listAccessible :: Conf -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAccessible conf path =
    listAccessible' conf id [path]
{-# INLINE listAccessible #-}


listEverythingAccessible :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listEverythingAccessible =
    listAccessible defConf
{-# INLINE listEverythingAccessible #-}


listDirectories :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listDirectories =
    listAccessible defConf{postCheck = \file _ -> Posix.isDirectory file}
{-# INLINE listDirectories #-}


listRegularFiles :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listRegularFiles =
    listAccessible defConf{postCheck = \file _ -> Posix.isRegularFile file}
{-# INLINE listRegularFiles #-}


listSymbolicLinks :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listSymbolicLinks =
    listAccessible defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
{-# INLINE listSymbolicLinks #-}
