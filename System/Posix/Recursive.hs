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
import Data.Foldable (fold)
import System.IO.Error (IOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Files (FileStatus)

import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix


foldMapA :: (Monoid b, Traversable t, Applicative f) => (a -> f b) -> t a -> f b
foldMapA = (fmap fold .) . traverse


{-# INLINE listDir #-}
listDir :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listDir predicate path =
    bracket
        (Posix.openDirStream path)
        Posix.closeDirStream
        (go [])
  where
    go :: [FilePath] -> Posix.DirStream -> IO [FilePath]
    go acc dirp = do
        e <- Posix.readDirStream dirp
        if null e
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
listAll' :: Bool -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
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


listAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listAll =
    listAll' False


followListAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
followListAll =
    listAll' True


listEverything :: FilePath -> IO [FilePath]
listEverything =
    listAll' False (const True)


followListEverything :: FilePath -> IO [FilePath]
followListEverything =
    listAll' True (const True)


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


{-# INLINE listAccessible' #-}
listAccessible' :: Conf -> FilePath -> IO [FilePath]
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


listAccessible :: Conf -> FilePath -> IO [FilePath]
listAccessible =
    listAccessible'


listEverythingAccessible :: FilePath -> IO [FilePath]
listEverythingAccessible =
    listAccessible' defConf


listDirectories :: FilePath -> IO [FilePath]
listDirectories =
    listAccessible' defConf{postCheck = \file _ -> Posix.isDirectory file}


listRegularFiles :: FilePath -> IO [FilePath]
listRegularFiles =
    listAccessible' defConf{postCheck = \file _ -> Posix.isRegularFile file}


listSymbolicLinks :: FilePath -> IO [FilePath]
listSymbolicLinks =
    listAccessible' defConf{postCheck = \file _ -> Posix.isSymbolicLink file}
