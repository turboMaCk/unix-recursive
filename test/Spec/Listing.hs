{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Listing (spec, DirectoryListing (..)) where

import Data.List (sort)
import Data.String
import System.Posix.Files (FileStatus)
import Test.Hspec

import qualified System.Posix.Files as Posix


class (Show a, Ord a, IsString a) => DirectoryListing a where
    listAll :: (a -> Bool) -> a -> IO [a]
    followListAll :: (a -> Bool) -> a -> IO [a]
    listEverything :: a -> IO [a]
    followListEverything :: a -> IO [a]
    listAccessible :: (a -> Bool) -> (FileStatus -> a -> Bool) -> a -> IO [a]
    listDirectories :: a -> IO [a]
    listRegularFiles :: a -> IO [a]
    listEverythingAccessible :: a -> IO [a]
    listSymbolicLinks :: a -> IO [a]


spec :: forall a. DirectoryListing a => (a -> a -> Bool) -> Spec
spec filter = do
    context "Not following symlinks" $ do
        describe "listEverything" $ do
            it "returns expected list of everything in workdir" $ do
                res :: [a] <- listEverything "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
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

        describe "listAll" $ do
            it "returns list excluding files & dirs ending with `3`" $ do
                res :: [a] <- listAll (not . filter "3") "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
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

        describe "listEverythingAccessible" $ do
            it "returns everything that is accessible in workdir" $ do
                res :: [a] <- listEverythingAccessible "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
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

        describe "listDirectories" $ do
            it "returns only directories" $ do
                res :: [a] <- listDirectories "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub2"
                               , "test/workdir/dir2"
                               , "test/workdir/dir3"
                               , "test/workdir/dir3/sub1"
                               , "test/workdir/dir3/sub2"
                               ]

        describe "listFiles" $ do
            it "returns only files" $ do
                res :: [a] <- listRegularFiles "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir/dir1/roots-dir1/roots-file1"
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

        describe "listSymbolicLinks" $ do
            it "returns only symlinks" $ do
                res :: [a] <- listSymbolicLinks "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir/dir2/sym1"
                               , "test/workdir/dir2/sym2"
                               , "test/workdir/dir3/sub2/broken-sym1"
                               ]

    describe "listAccessible" $ do
        it "return everything except for symlinks and not within dir1" $ do
            res :: [a] <-
                listAccessible
                    (not . filter "dir1")
                    (\file _ -> not (Posix.isSymbolicLink file))
                    "test/workdir"
            sort res
                `shouldBe` [ "test/workdir"
                           , "test/workdir/dir2"
                           , "test/workdir/dir2/file1"
                           , "test/workdir/dir3"
                           , "test/workdir/dir3/sub1"
                           , "test/workdir/dir3/sub1/file1"
                           , "test/workdir/dir3/sub2"
                           , "test/workdir/file1"
                           , "test/workdir/file2"
                           ]

    context "Following symlinks" $ do
        describe "followListEverything" $ do
            it "returns expected list of everything in workdir" $ do
                res :: [a] <- followListEverything "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
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

        describe "followListAll" $ do
            it "returns list excluding files & dirs ending with `3`" $ do
                res <- followListAll (not . filter "3") "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
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
