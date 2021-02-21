{- |
 Module      : System.Posix.Recursive.ByteString-- Copyright   : (c) Marek Fajkus
 License     : BSD3

 Maintainer  : marek.faj@gmail.com

 All modules profided by @unix-recursive@ expose similar API.
 Make sure you're using the module wich best fits your needs based on:
   - Working  with 'RawFilePath' (faster and more packed) or 'FilePath' (slower but easier to work with safely)
   - Exception free (Default) or @Unsafe@ variants of functions

 = Usage

 This module is designed to be imported as @qualified@:

 > import qualified System.Posix.Recursive.ByteString as PosixRecursive

 __Results__

 All functions return will return root path (the one they accept in argument) as a first item in the list:

 > head <$> PosixRecursive.list "System"
 > >>> "System"

 Other than that, this library __provides no guarantees about the order in which files appear in the resulting list__
 to make it possible to change the underlaying strategy in future versions.

 __Laziness__

 All IO operations are __guaranteed to be lazy and have constanct space characteristic__.
 Only the IO required by lazy computation will be performed so for instance running code like:

 > take 20 <$> PosixRecursive.listDirectories "/"

 Will perform only minimal ammount of IO needed to collect 20 directories on a root partition
-}
module System.Posix.Recursive.ByteString (
    -- * Basic Listing
    -- $basic_listing
    list,
    followList,
    listMatching,
    followListMatching,

    -- * File Type Based Listing
    -- $type_based
    listAccessible,
    listDirectories,
    listRegularFiles,
    listSymbolicLinks,

    -- * Custom Listing
    -- $custom
    Conf (..),
    defConf,
    listCustom,
) where

import Control.Exception (bracket, handle)
import Data.Foldable (fold)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (FileStatus)

import qualified Data.ByteString as BS
import System.Posix.ByteString.FilePath (RawFilePath)

import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix


-- Helpers

foldMapA :: (Monoid b, Traversable t, Applicative f) => (a -> f b) -> t a -> f b
foldMapA = (fmap fold .) . traverse


{-# INLINE listDir #-}
listDir :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listDir predicate path =
    bracket
        (Posix.openDirStream path)
        Posix.closeDirStream
        (go [])
  where
    go :: [RawFilePath] -> Posix.DirStream -> IO [RawFilePath]
    go acc dirp = do
        e <- Posix.readDirStream dirp
        if BS.null e
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

  __Directories (and files) which process can't open due to permissions are listed but not recursed into.__

 Functions from this section is gurantee to always return the root path as a first element even
 if this path does not exist.

 > PosixRecursive.list "i-dont-exist"
 > >>> ["i-dont-exist"]

 In these cases the root path is considered the same way as symlink
 to non existing location.
-}


{-# INLINE listAll' #-}
listAll' :: Bool -> (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listAll' followSymlinks predicate path =
    handle (\(_ :: IOError) -> pure []) $ do
        file <- getFileStatus path
        if Posix.isDirectory file
            then do
                content <- listDir predicate path

                next <- unsafeInterleaveIO $ foldMapA (listAll' followSymlinks predicate) content
                pure $ content ++ next
            else pure []
  where
    {-# INLINE getFileStatus #-}
    getFileStatus
        | followSymlinks = Posix.getFileStatus
        | otherwise = Posix.getSymbolicLinkStatus


{-# INLINE listAll'' #-}
listAll'' :: Bool -> (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listAll'' followSymlinks predicate path =
    (path :) <$> listAll' followSymlinks predicate path


-- | Like 'list' but uses provided function to test in which 'RawFilePath' to recurse into.
listMatching :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listMatching =
    listAll'' False


-- | Like 'followList' but uses provided function to test in which 'RawFilePath' to recurse into.
followListMatching :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
followListMatching =
    listAll'' True


{- | List all files, directories & symlinks recursively.
 Symlinks are not followed. See 'followList'.
-}
list :: RawFilePath -> IO [RawFilePath]
list =
    listAll'' False (const True)


{- | List all files, directories & symlinks recursively.
 Unlike 'list', symlinks are followed recursively as well.
-}
followList :: RawFilePath -> IO [RawFilePath]
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


-- | Configuration arguments for 'listCustom'.
data Conf = Conf
    { -- | Filter paths algorithm should recurse into
      filterPath :: !(RawFilePath -> Bool)
    , -- | Test if file should be included in returned List
      includeFile :: !(FileStatus -> RawFilePath -> IO Bool)
    , -- | Follow symbolic links
      followSymlinks :: !Bool
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


{-# INLINE listAccessible' #-}
listAccessible' :: Conf -> RawFilePath -> IO [RawFilePath]
listAccessible' Conf{..} path =
    handle (\(_ :: IOError) -> pure []) $ do
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


-- | Recursively list files using custom filters.
listCustom :: Conf -> RawFilePath -> IO [RawFilePath]
listCustom =
    listAccessible'


{- | Like 'list' but automatically filters out inacessible files like broken symlins
or unreadable files and directories.
-}
listAccessible :: RawFilePath -> IO [RawFilePath]
listAccessible =
    listAccessible' defConf


{- $type_based
 Functions for listing specific file type. Reading the file type requires
 ability to read the file.

  __These function do not return broken symlinks and inacessible files__

 Include test is applied even for the root entry (path past in as an argument).
 This means that non existing paths are filtered.

 > PosixRecursive.listDirectories "i-dont-exist"
 > >>> []
-}


-- | List sub directories of given directory.
listDirectories :: RawFilePath -> IO [RawFilePath]
listDirectories =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isDirectory file}


-- | Lists only files (while recursing into directories still).
listRegularFiles :: RawFilePath -> IO [RawFilePath]
listRegularFiles =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isRegularFile file}


-- | Lists only symbolic links (while recursing into directories still).
listSymbolicLinks :: RawFilePath -> IO [RawFilePath]
listSymbolicLinks =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isSymbolicLink file}
