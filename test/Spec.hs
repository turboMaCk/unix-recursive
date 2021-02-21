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
    list = String.list
    followList = String.followList
    listMatching = String.listMatching
    followListMatching = String.followListMatching
    listAccessible = String.listAccessible
    listDirectories = String.listDirectories
    listRegularFiles = String.listRegularFiles
    listSymbolicLinks = String.listSymbolicLinks
    listCustom filterPath includeFile =
        String.listCustom
            String.defConf
                { String.filterPath = filterPath
                , String.includeFile = includeFile
                }


instance DirectoryListing RawFilePath where
    list = ByteString.list
    followList = ByteString.followList
    listMatching = ByteString.listMatching
    followListMatching = ByteString.followListMatching
    listDirectories = ByteString.listDirectories
    listAccessible = ByteString.listAccessible
    listRegularFiles = ByteString.listRegularFiles
    listSymbolicLinks = ByteString.listSymbolicLinks
    listCustom filterPath includeFile =
        ByteString.listCustom
            ByteString.defConf
                { ByteString.filterPath = filterPath
                , ByteString.includeFile = includeFile
                }


main :: IO ()
main =
    hspec $ do
        context "System.Posix.Recursive" $ spec isSuffixOf

        context "System.Posix.Recursive.Unsafe" $
            it "should throw" $ do
                let exp = do
                        res <- Unsafe.list "test/workdir"
                        print $ length res
                exp `shouldThrow` isPermissionError

        context "System.Posix.Recursive.ByteString" $ spec BS.isSuffixOf

        context "System.Posix.Recursive.ByteString.Unsafe" $
            it "should throw" $ do
                let exp = do
                        res <- UnsafeBS.list "test/workdir"
                        print $ length res
                exp `shouldThrow` isPermissionError
