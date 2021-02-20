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
import Data.Foldable (fold)
import System.IO.Error (IOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (FileStatus)

import qualified Data.ByteString as BS
import System.Posix.ByteString.FilePath (RawFilePath)

import qualified System.Posix.Directory.ByteString as Posix
import qualified System.Posix.Files.ByteString as Posix


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


listAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
listAll =
    listAll' False


followListAll :: (RawFilePath -> Bool) -> RawFilePath -> IO [RawFilePath]
followListAll =
    listAll' True


listEverything :: RawFilePath -> IO [RawFilePath]
listEverything =
    listAll' False (const True)


followListEverything :: RawFilePath -> IO [RawFilePath]
followListEverything =
    listAll' True (const True)


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


{-# INLINE listAccessible' #-}
listAccessible' :: Conf -> RawFilePath -> IO [RawFilePath]
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


listAccessible :: Conf -> RawFilePath -> IO [RawFilePath]
listAccessible =
    listAccessible'


listEverythingAccessible :: RawFilePath -> IO [RawFilePath]
listEverythingAccessible =
    listAccessible' defConf


listDirectories :: RawFilePath -> IO [RawFilePath]
listDirectories =
    listAccessible' defConf{postCheck = \file _ -> Posix.isDirectory file}


listRegularFiles :: RawFilePath -> IO [RawFilePath]
listRegularFiles =
    listAccessible' defConf{postCheck = \file _ -> Posix.isRegularFile file}


listSymbolicLinks :: RawFilePath -> IO [RawFilePath]
listSymbolicLinks =
    listAccessible' defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
