{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main

import System.Directory.Recursive as DirTraverse
import qualified System.Posix.Recursive as String
import qualified System.Posix.Recursive.ByteString as ByteString


main :: IO ()
main =
    defaultMain
        [ bgroup
            "compare different api groups"
            [ bgroup
                "ByteString"
                [ bench "filtering base on path" $ nfIO $ ByteString.listEverything "."
                , bench "filter by path and file status" $ nfIO $ ByteString.listEverythingAccessible "."
                ]
            , bgroup
                "String"
                [ bench "filtering base on path" $ nfIO $ String.listEverything "."
                , bench "filter by path and file status" $ nfIO $ String.listEverythingAccessible "."
                ]
            ]
        , bgroup
            "compare with dir traverse"
            [ bgroup
                "Evaluate whole list"
                [ bench "unix-recursive String" $ nfIO $ String.listEverything "."
                , bench "unix-recursive String dirs only" $ nfIO $ String.listDirectories "."
                , bench "unix-recursive ByteString" $ nfIO $ ByteString.listEverything "."
                , bench "unix-recursive ByteString dirs only" $ nfIO $ ByteString.listDirectories "."
                , bench "dir-traverse" $ nfIO $ DirTraverse.getDirRecursive "."
                ]
            , bgroup
                "Test laziness"
                [ bench "unix-recursive String" $ nfIO (head . tail <$> String.listEverything ".")
                , bench "unix-recursive String dirs only" $ nfIO (head . tail <$> String.listDirectories ".")
                , bench "unix-recursive ByteString" $ nfIO (head . tail <$> ByteString.listEverything ".")
                , bench "unix-recursive ByteString dirs only" $ nfIO (head . tail <$> ByteString.listDirectories ".")
                , bench "dir-traverse" $ nfIO ((head . tail) <$> DirTraverse.getDirRecursive ".")
                ]
            ]
        ]
