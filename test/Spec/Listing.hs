{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Listing (spec, DirectoryListing (..)) where

import Data.List (sort)
import Data.String
import System.Posix.Files (FileStatus)
import qualified System.Posix.Files as Posix
import Test.Hspec


class (Show a, Ord a, IsString a) => DirectoryListing a where
    list :: a -> IO [a]
    followList :: a -> IO [a]
    listMatching :: (a -> Bool) -> a -> IO [a]
    followListMatching :: (a -> Bool) -> a -> IO [a]
    listAccessible :: a -> IO [a]
    listDirectories :: a -> IO [a]
    listRegularFiles :: a -> IO [a]
    listSymbolicLinks :: a -> IO [a]
    listCustom :: (a -> Bool) -> (FileStatus -> a -> IO Bool) -> a -> IO [a]


spec :: forall a. DirectoryListing a => (a -> a -> Bool) -> Spec
spec filter = do
    context "Not following symlinks" $ do
        describe "listEverything" $ do
            it "returns expected list of everything in workdir" $ do
                res :: [a] <- list "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/file3"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/dir3"
                        , "test/workdir/dir3/sub1"
                        , "test/workdir/dir3/sub1/file1"
                        , "test/workdir/dir3/sub2"
                        , "test/workdir/dir3/sub2/broken-sym1"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        , "test/workdir/only-roots-dir1"
                        ]
                res `shouldMatchList` expected

        describe "listAll" $ do
            it "returns list excluding files & dirs ending with `3`" $ do
                res :: [a] <- listMatching (not . filter "3") "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        , "test/workdir/only-roots-dir1"
                        ]
                res `shouldMatchList` expected

        describe "listEverythingAccessible" $ do
            it "returns everything that is accessible in workdir" $ do
                res :: [a] <- listAccessible "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/file3"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/dir3"
                        , "test/workdir/dir3/sub1"
                        , "test/workdir/dir3/sub1/file1"
                        , "test/workdir/dir3/sub2"
                        , "test/workdir/dir3/sub2/broken-sym1"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        ]
                res `shouldMatchList` expected

        describe "listDirectories" $ do
            it "returns only directories" $ do
                res :: [a] <- listDirectories "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir2"
                        , "test/workdir/dir3"
                        , "test/workdir/dir3/sub1"
                        , "test/workdir/dir3/sub2"
                        ]
                res `shouldMatchList` expected

        describe "listFiles" $ do
            it "returns only files" $ do
                res :: [a] <- listRegularFiles "test/workdir"
                let expected =
                        [ "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/file3"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir3/sub1/file1"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        ]
                res `shouldMatchList` expected

        describe "listSymbolicLinks" $ do
            it "returns only symlinks" $ do
                res :: [a] <- listSymbolicLinks "test/workdir"
                let expected =
                        [ "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/dir3/sub2/broken-sym1"
                        ]
                res `shouldMatchList` expected

        describe "listCustom" $ do
            it "return everything except for symlinks and not within dir1" $ do
                res :: [a] <-
                    listCustom
                        (not . filter "dir1")
                        (\file _ -> pure $ not $ Posix.isSymbolicLink file)
                        "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir3"
                        , "test/workdir/dir3/sub1"
                        , "test/workdir/dir3/sub1/file1"
                        , "test/workdir/dir3/sub2"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        ]
                res `shouldMatchList` expected

    context "Following symlinks" $ do
        describe "followListEverything" $ do
            it "returns expected list of everything in workdir" $ do
                res :: [a] <- followList "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/file3"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/dir2/sym2/sub1"
                        , "test/workdir/dir2/sym2/sub1/file1"
                        , "test/workdir/dir2/sym2/sub2"
                        , "test/workdir/dir2/sym2/sub2/broken-sym1"
                        , "test/workdir/dir3"
                        , "test/workdir/dir3/sub1"
                        , "test/workdir/dir3/sub1/file1"
                        , "test/workdir/dir3/sub2"
                        , "test/workdir/dir3/sub2/broken-sym1"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        , "test/workdir/only-roots-dir1"
                        ]
                res `shouldMatchList` expected

        describe "followListAll" $ do
            it "returns list excluding files & dirs ending with `3`" $ do
                res <- followListMatching (not . filter "3") "test/workdir"
                let expected =
                        [ "test/workdir"
                        , "test/workdir/dir1"
                        , "test/workdir/dir1/roots-dir1"
                        , "test/workdir/dir1/roots-dir1/roots-file1"
                        , "test/workdir/dir1/sub1"
                        , "test/workdir/dir1/sub1/file1"
                        , "test/workdir/dir1/sub1/file2"
                        , "test/workdir/dir1/sub2"
                        , "test/workdir/dir1/sub2/file1"
                        , "test/workdir/dir1/sub2/file2"
                        , "test/workdir/dir1/sub2/roots-file1"
                        , "test/workdir/dir2"
                        , "test/workdir/dir2/file1"
                        , "test/workdir/dir2/sym1"
                        , "test/workdir/dir2/sym2"
                        , "test/workdir/dir2/sym2/sub1"
                        , "test/workdir/dir2/sym2/sub1/file1"
                        , "test/workdir/dir2/sym2/sub2"
                        , "test/workdir/dir2/sym2/sub2/broken-sym1"
                        , "test/workdir/file1"
                        , "test/workdir/file2"
                        , "test/workdir/only-roots-dir1"
                        ]
                res `shouldMatchList` expected
