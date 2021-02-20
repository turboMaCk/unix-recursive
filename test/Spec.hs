{-# LANGUAGE FlexibleInstances #-}

import Data.List (sort)

import Spec.Listing
import Test.Hspec (context, hspec)

import System.Posix.ByteString.FilePath (RawFilePath)

import qualified Data.ByteString as BS
import qualified System.Posix.Recursive as String
import qualified System.Posix.Recursive.ByteString as ByteString


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
    listAccessible filterPath includeFile =
        String.listAccessible
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
    listAccessible filterPath includeFile =
        ByteString.listAccessible
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
        context "FilePath (String) API" $ spec isSuffixOf
        context "RawFilePath (ByteString) API" $ spec BS.isSuffixOf
