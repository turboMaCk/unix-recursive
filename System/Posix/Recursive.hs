module System.Posix.Recursive (
    Conf (..),
    defConf,
    listAll,
    followListAll,
    listEverything,
    followListEverything,
    listAccessible,
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

import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix


-- Helpers

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


-- List all (filtering based on path)

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


{-# INLINE listAll'' #-}
listAll'' :: Bool -> (FilePath -> Bool) -> FilePath -> IO [FilePath]
listAll'' followSymlinks predicate path =
    (path :) <$> listAll' followSymlinks predicate path


listAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listAll =
    listAll'' False


followListAll :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
followListAll =
    listAll'' True


listEverything :: FilePath -> IO [FilePath]
listEverything =
    listAll'' False (const True)


followListEverything :: FilePath -> IO [FilePath]
followListEverything =
    listAll'' True (const True)


-- List accessible (filtering based on both path as well as file status)

data Conf = Conf
    { filterPath :: !(FilePath -> Bool)
    , includeFile :: !(FileStatus -> FilePath -> IO Bool)
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
listAccessible' :: Conf -> FilePath -> IO [FilePath]
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


listAccessible :: Conf -> FilePath -> IO [FilePath]
listAccessible =
    listAccessible'


listEverythingAccessible :: FilePath -> IO [FilePath]
listEverythingAccessible =
    listAccessible' defConf


listDirectories :: FilePath -> IO [FilePath]
listDirectories =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isDirectory file}


listRegularFiles :: FilePath -> IO [FilePath]
listRegularFiles =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isRegularFile file}


listSymbolicLinks :: FilePath -> IO [FilePath]
listSymbolicLinks =
    listAccessible' defConf{includeFile = \file _ -> pure $ Posix.isSymbolicLink file}
