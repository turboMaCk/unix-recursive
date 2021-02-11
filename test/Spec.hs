{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Test.Hspec

import qualified Data.ByteString as BS
import qualified System.Posix.Files.ByteString as Posix
import qualified System.Posix.Recursive as Lib


main :: IO ()
main =
    hspec $ do
        describe "listEverything" $ do
            it "returns expected list of everything in workdir" $ do
                res <- Lib.listEverything "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/roots-dir1/roots-file1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub1/file1"
                               , "test/workdir/dir1/sub1/file2"
                               , "test/workdir/dir1/sub1/subsub1"
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
                               , "test/workdir/dir2/sym2/sub2"
                               , "test/workdir/dir2/sym2/sub2/broken-sym1"
                               , "test/workdir/dir3"
                               , "test/workdir/dir3/sub1"
                               , "test/workdir/dir3/sub2"
                               , "test/workdir/dir3/sub2/broken-sym1"
                               , "test/workdir/file1"
                               , "test/workdir/file2"
                               , "test/workdir/only-roots-dir1"
                               ]

        describe "listAll" $ do
            it "returns list excluding files & dirs ending with `3`" $ do
                res <- Lib.listAll (not . ("3" `BS.isSuffixOf`)) "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/roots-dir1/roots-file1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub1/file1"
                               , "test/workdir/dir1/sub1/file2"
                               , "test/workdir/dir1/sub1/subsub1"
                               , "test/workdir/dir1/sub2"
                               , "test/workdir/dir1/sub2/file1"
                               , "test/workdir/dir1/sub2/file2"
                               , "test/workdir/dir1/sub2/roots-file1"
                               , "test/workdir/dir2"
                               , "test/workdir/dir2/file1"
                               , "test/workdir/dir2/sym1"
                               , "test/workdir/dir2/sym2"
                               , "test/workdir/dir2/sym2/sub1"
                               , "test/workdir/dir2/sym2/sub2"
                               , "test/workdir/dir2/sym2/sub2/broken-sym1"
                               , "test/workdir/file1"
                               , "test/workdir/file2"
                               , "test/workdir/only-roots-dir1"
                               ]

        describe "listEverythingAccessible" $ do
            it "returns everything that is accessible in workdir" $ do
                res <- Lib.listEverythingAccessible "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/roots-dir1/roots-file1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub1/file1"
                               , "test/workdir/dir1/sub1/file2"
                               , "test/workdir/dir1/sub1/subsub1"
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
                               , "test/workdir/dir2/sym2/sub2"
                               , "test/workdir/dir3"
                               , "test/workdir/dir3/sub1"
                               , "test/workdir/dir3/sub2"
                               , "test/workdir/file1"
                               , "test/workdir/file2"
                               ]

        describe "listDirectories" $ do
            it "returns only directories" $ do
                res <- Lib.listDirectories "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub1/subsub1"
                               , "test/workdir/dir1/sub2"
                               , "test/workdir/dir2"
                               , "test/workdir/dir2/sym2"
                               , "test/workdir/dir2/sym2/sub1"
                               , "test/workdir/dir2/sym2/sub2"
                               , "test/workdir/dir3"
                               , "test/workdir/dir3/sub1"
                               , "test/workdir/dir3/sub2"
                               ]

        describe "listFiles" $ do
            it "returns only files" $ do
                res <- Lib.listRegularFiles "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir/dir1/roots-dir1/roots-file1"
                               , "test/workdir/dir1/sub1/file1"
                               , "test/workdir/dir1/sub1/file2"
                               , "test/workdir/dir1/sub2/file1"
                               , "test/workdir/dir1/sub2/file2"
                               , "test/workdir/dir1/sub2/file3"
                               , "test/workdir/dir1/sub2/roots-file1"
                               , "test/workdir/dir2/file1"
                               , "test/workdir/dir2/sym1"
                               , "test/workdir/file1"
                               , "test/workdir/file2"
                               ]

        describe "listAccessible" $ do
            it "return everything except for symlinks" $ do
                res <-
                    Lib.listAccessible
                        ( Lib.defConf
                            { Lib.postCheck = \file _ -> not (Posix.isSymbolicLink file)
                            , Lib.followSymlinks = False
                            }
                        )
                        "test/workdir"
                sort res
                    `shouldBe` [ "test/workdir"
                               , "test/workdir/dir1"
                               , "test/workdir/dir1/roots-dir1"
                               , "test/workdir/dir1/roots-dir1/roots-file1"
                               , "test/workdir/dir1/sub1"
                               , "test/workdir/dir1/sub1/file1"
                               , "test/workdir/dir1/sub1/file2"
                               , "test/workdir/dir1/sub1/subsub1"
                               , "test/workdir/dir1/sub2"
                               , "test/workdir/dir1/sub2/file1"
                               , "test/workdir/dir1/sub2/file2"
                               , "test/workdir/dir1/sub2/file3"
                               , "test/workdir/dir1/sub2/roots-file1"
                               , "test/workdir/dir2"
                               , "test/workdir/dir2/file1"
                               , "test/workdir/dir3"
                               , "test/workdir/dir3/sub1"
                               , "test/workdir/dir3/sub2"
                               , "test/workdir/file1"
                               , "test/workdir/file2"
                               ]
