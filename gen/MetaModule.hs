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
import Data.Foldable (fold)
import System.IO.Error (IOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (FileStatus)

<? echo $imports ?>


foldMapA  :: (Monoid b, Traversable t, Applicative f) => (a -> f b) -> t a -> f b
foldMapA = (fmap fold .) . traverse


{-# INLINE listDir #-}
listDir :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listDir predicate path =
    bracket
        (Posix.openDirStream path)
        Posix.closeDirStream
        (go [])
  where
    go :: [<? echo $file_path_type ?>] -> Posix.DirStream -> IO [<? echo $file_path_type ?>]
    go acc dirp = do
        e <- Posix.readDirStream dirp
        if <? echo $check_empty_fc ?> e
            then pure acc
            else
                if e /= "." && e /= ".."
                    then
                        let fullPath = path <> "/" <> e
                         in if predicate fullPath
                                then go (fullPath : acc) dirp
                                else go acc dirp
                    else go acc dirp


{-# INLINE listAll' #-}
listAll' :: Bool -> (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAll' followSymlinks predicate path =
    handle (\(_ :: IOError) -> pure []) $ do
        file <- getFileStatus path
        if Posix.isDirectory file
           then do
               content <- listDir predicate path

               next <- unsafeInterleaveIO $ foldMapA (listAll' followSymlinks predicate) content
               pure $ content ++ next
           else
               pure []
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus


listAll :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAll =
    listAll' False


followListAll :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followListAll =
    listAll' True


listEverything :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listEverything =
    listAll' False (const True)


followListEverything :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followListEverything =
    listAll' True (const True)


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


{-# INLINE listAccessible' #-}
listAccessible' :: Conf -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAccessible' Conf{..} path =
    handle (\(_ :: IOError) -> pure []) $ do
        file <- getFileStatus path
        next <-
            if Posix.isDirectory file
                then do
                    content <- listDir preCheck path
                    unsafeInterleaveIO $ foldMapA (listAccessible' Conf{..}) content
                else pure []

        if postCheck file path
            then pure $ path : next
            else pure next
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus


listAccessible :: Conf -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAccessible =
    listAccessible'


listEverythingAccessible :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listEverythingAccessible =
    listAccessible' defConf


listDirectories :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listDirectories =
    listAccessible' defConf{postCheck = \file _ -> Posix.isDirectory file}


listRegularFiles :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listRegularFiles =
    listAccessible' defConf{postCheck = \file _ -> Posix.isRegularFile file}


listSymbolicLinks :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listSymbolicLinks =
    listAccessible' defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
