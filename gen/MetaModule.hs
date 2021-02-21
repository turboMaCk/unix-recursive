-- |
-- Module      : <? echo $module_name ?>
-- Copyright   : (c) Marek Fajkus
-- License     : BSD3
--
-- Maintainer  : marek.faj@gmail.com
--
-- All modules profided by @unix-recursive@ expose similar API.
-- Make sure you're using the module wich best fits your needs based on:
--   - Working  with 'RawFilePath' (faster and more packed) or 'FilePath' (slower but easier to work with safely)
--   - Exception free (Default) or @Unsafe@ variants of functions
--
-- = Usage
--
-- This module is designed to be imported as @qualified@:
--
-- > import qualified <? echo $module_name ?> as PosixRecursive
--
-- __Results__
--
-- All functions return will return root path (the one they accept in argument) as a first item in the list:
--
-- > head <$> PosixRecursive.list "System"
-- > >>> "System"
--
-- Other than that, this library __provides no guarantees about the order in which files appear in the resulting list__
-- to make it possible to change the underlaying strategy in future versions.
--
-- __Laziness__
--
-- All IO operations are __guaranteed to be lazy and have constanct space characteristic__.
-- Only the IO required by lazy computation will be performed so for instance running code like:
--
-- > take 20 <$> PosixRecursive.listDirectories "/"
--
-- Will perform only minimal ammount of IO needed to collect 20 directories on a root partition

module <? echo $module_name ?> (
    -- * Basic Listing
    -- $basic_listing
    list,
    followList,
    listMatching,
    followListMatching,
    -- * File Type Based Listing
    -- $type_based
    <? if ($safe) { ?>
    listAccessible,
    <? } ?>
    listDirectories,
    listRegularFiles,
    listSymbolicLinks,
    -- * Custom Listing
    -- $custom
    Conf (..),
    defConf,
    listCustom,
) where

<? if ($safe) { ?>
import Control.Exception (bracket, handle)
import System.Posix.Files (FileStatus)
<? } else { ?>
import Control.Exception (bracket)
<? } ?>
import Data.Foldable (fold)
import System.IO.Unsafe (unsafeInterleaveIO)

<? echo $imports ?>


-- Helpers

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


{- $basic_listing
 Functions for listing contents of directory recursively.
 These functions list all the content they encounter while traversing
 the file system tree including directories, files, symlinks, broken symlinks.

 <? if ($safe) { ?>
 __Directories (and files) which process can't open due to permissions are listed but not recursed into.__
 <? } else { ?>
 __Functions from this module will throw 'IOError' if it can't open the directory__
 (i.e. becacuse permission error or other process removing the given path).
 <? } ?>

 Functions from this section is gurantee to always return the root path as a first element even
 if this path does not exist.

 > PosixRecursive.list "i-dont-exist"
 > >>> ["i-dont-exist"]

 In these cases the root path is considered the same way as symlink
 to non existing location.
-}

{-# INLINE listAll' #-}
listAll' :: Bool -> (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAll' followSymlinks predicate path =
    <? if ($safe) { ?>handle (\(_ :: IOError) -> pure []) $ <? } ?> do
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


{-# INLINE listAll'' #-}
listAll'' :: Bool -> (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAll'' followSymlinks predicate path =
    (path :) <$> listAll' followSymlinks predicate path


{- | Like 'list' but uses provided function to test in which '<? echo $file_path_type ?>' to recurse into.
-}
listMatching :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listMatching =
    listAll'' False


{- | Like 'followList' but uses provided function to test in which '<? echo $file_path_type ?>' to recurse into.
-}
followListMatching :: (<? echo $file_path_type ?> -> Bool) -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followListMatching =
    listAll'' True


{- | List all files, directories & symlinks recursively.
 Symlinks are not followed. See 'followList'.
-}
list :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
list =
    listAll'' False (const True)


{- | List all files, directories & symlinks recursively.
 Unlike 'list', symlinks are followed recursively as well.
-}
followList :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
followList =
    listAll'' True (const True)


{- $custom
 All /File Type Based Listing/ functions are based on top of this interface.
 This part of API exposes exposes access for writing custom filtering functions.

 All paths are tested for filter functions so unreadble files won't appear in the result list:

 > PosixRecursive.listCustom PosixRecursive.defConf "i-dont-exist"
 > >>> []

 It's not possible to turn of this behaviour because this functions must get the 'FileStatus'
 type which requires reading each entry.
-}

<? if ($safe) { ?>
{- | Configuration arguments for 'listCustom'.
-}
data Conf = Conf
    { filterPath :: !(<? echo $file_path_type ?> -> Bool) -- ^ Filter paths algorithm should recurse into
    , includeFile :: !(FileStatus -> <? echo $file_path_type ?> -> IO Bool) -- ^ Test if file should be included in returned List
    , followSymlinks :: !Bool -- ^ Follow symbolic links
    }


{- | Default 'Conf'iguration.

> listCustom defConf == listAccessible

* Recurses into all Paths
* Includes all file types
* Does __not__ follow symlinks
-}
defConf :: Conf
defConf =
    Conf
        { filterPath = const True
        , includeFile = \_ _ -> pure True
        , followSymlinks = False
        }
<? } ?>


{-# INLINE listAccessible' #-}
listAccessible' :: Conf -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAccessible' Conf{..} path =
    <? if ($safe) { ?>handle (\(_ :: IOError) -> pure []) $ <? } ?> do
        file <- getFileStatus path
        next <-
            if Posix.isDirectory file
                then do
                    content <- listDir filterPath path
                    unsafeInterleaveIO $ foldMapA (listAccessible' Conf{..}) content
                else pure []

        include <- includeFile file path
        if include
            then pure $ path : next
            else pure next
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus


{- | Recursively list files using custom filters.
-}
listCustom :: Conf -> <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listCustom =
    listAccessible'


<? if ($safe) { ?>
{- | Like 'list' but automatically filters out inacessible files like broken symlins
or unreadable files and directories.
-}
listAccessible :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listAccessible =
    listAccessible' defConf
<? } ?>


{- $type_based
 Functions for listing specific file type. Reading the file type requires
 ability to read the file.

 <? if ($safe) { ?>
 __These function do not return broken symlinks and inacessible files__
 <? } else { ?>
 __These functions will throw 'IOError' when tring to open unreadable file.__
 <? } ?>

 Include test is applied even for the root entry (path past in as an argument).
 This means that non existing paths are filtered.

 > PosixRecursive.listDirectories "i-dont-exist"
 > >>> []
-}


{- | List sub directories of given directory.
-}
listDirectories :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listDirectories =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isDirectory file}


{- | Lists only files (while recursing into directories still).
-}
listRegularFiles :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listRegularFiles =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isRegularFile file}


{- | Lists only symbolic links (while recursing into directories still).
-}
listSymbolicLinks :: <? echo $file_path_type ?> -> IO [<? echo $file_path_type ?>]
listSymbolicLinks =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isSymbolicLink file}
