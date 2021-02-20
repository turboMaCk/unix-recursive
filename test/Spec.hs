{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)

import Spec.Listing
import Test.Hspec --(anyException, context, hspec, it, shouldThrow)

import System.IO.Error (isPermissionError)
import System.Posix.ByteString.FilePath (RawFilePath)

import qualified Data.ByteString as BS
import qualified System.Posix.Recursive as String
import qualified System.Posix.Recursive.ByteString as ByteString
import qualified System.Posix.Recursive.ByteString.Unsafe as UnsafeBS
import qualified System.Posix.Recursive.Unsafe as Unsafe


-- Helper
isSuffixOf :: String -> String -> Bool
isSuffixOf needle haystack
    | needleLen > hayLen = False
    | otherwise = needle == drop (hayLen - needleLen) haystack
  where
    needleLen = length needle
    hayLen = length haystack


instance DirectoryListing FilePath where
    listAll = String.listAll
    followListAll = String.followListAll
    listEverything = String.listEverything
    followListEverything = String.followListEverything
    listCustom filterPath includeFile =
        String.listCustom
            String.defConf
                { String.filterPath = filterPath
                , String.includeFile = includeFile
                }
    listDirectories = String.listDirectories
    listRegularFiles = String.listRegularFiles
    listEverythingAccessible = String.listEverythingAccessible
    listSymbolicLinks = String.listSymbolicLinks


instance DirectoryListing RawFilePath where
    listAll = ByteString.listAll
    followListAll = ByteString.followListAll
    listEverything = ByteString.listEverything
    followListEverything = ByteString.followListEverything
    listCustom filterPath includeFile =
        ByteString.listCustom
            ByteString.defConf
                { ByteString.filterPath = filterPath
                , ByteString.includeFile = includeFile
                }
    listDirectories = ByteString.listDirectories
    listRegularFiles = ByteString.listRegularFiles
    listEverythingAccessible = ByteString.listEverythingAccessible
    listSymbolicLinks = ByteString.listSymbolicLinks


main :: IO ()
main =
    hspec $ do
        context "System.Posix.Recursive" $ spec isSuffixOf

        context "System.Posix.Recursive.Unsafe" $
            it "should throw" $ do
                let exp = do
                        res <- Unsafe.listEverything "test/workdir"
                        print $ length res
                exp `shouldThrow` isPermissionError

        context "System.Posix.Recursive.ByteString" $ spec BS.isSuffixOf

        context "System.Posix.Recursive.ByteString.Unsafe" $
            it "should throw" $ do
                let exp = do
                        res <- UnsafeBS.listEverything "test/workdir"
                        print $ length res
                exp `shouldThrow` isPermissionError
