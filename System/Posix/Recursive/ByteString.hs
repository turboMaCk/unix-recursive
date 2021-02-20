module System.Posix.Recursive.ByteString (
    Conf (..),
    defConf,
    listAll,
    followListAll,
    listEverything,
    followListEverything,
    listCustom,
    listEverythingAccessible,
    listDirectories,
    listRegularFiles,
    listSymbolicLinks,
) where

import Control.Exception (bracket, handle)
import Data.Foldable (fold)
import System.IO.Error (IOError)
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


-- List all (filtering based on path)

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


listAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listAll =
    listAll'' False


followListAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
followListAll =
    listAll'' True


listEverything :: RawFilePath -> IO [RawFilePath]
listEverything =
    listAll'' False (const True)


followListEverything :: RawFilePath -> IO [RawFilePath]
followListEverything =
    listAll'' True (const True)


-- List accessible (filtering based on both path as well as file status)

data Conf = Conf
    { filterPath :: !(RawFilePath -> Bool)
    , includeFile :: !(FileStatus -> RawFilePath -> IO Bool)
    , followSymlinks :: !Bool
    }


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


listCustom :: Conf -> RawFilePath -> IO [RawFilePath]
listCustom =
    listAccessible'


listEverythingAccessible :: RawFilePath -> IO [RawFilePath]
listEverythingAccessible =
    listAccessible' defConf


listDirectories :: RawFilePath -> IO [RawFilePath]
listDirectories =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isDirectory file}


listRegularFiles :: RawFilePath -> IO [RawFilePath]
listRegularFiles =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isRegularFile file}


listSymbolicLinks :: RawFilePath -> IO [RawFilePath]
listSymbolicLinks =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isSymbolicLink file}
