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
                [ bench "filtering base on path" $ nfIO $ ByteString.list "."
                , bench "filter by path and file status" $ nfIO $ ByteString.listAccessible "."
                ]
            , bgroup
                "String"
                [ bench "filtering base on path" $ nfIO $ String.list "."
                , bench "filter by path and file status" $ nfIO $ String.listAccessible "."
                ]
            ]
        , bgroup
            "compare with dir traverse"
            [ bgroup
                "Evaluate whole list"
                [ bench "unix-recursive String" $ nfIO $ String.followList "."
                , bench "unix-recursive ByteString" $ nfIO $ ByteString.followList "."
                , bench "dir-traverse" $ nfIO $ DirTraverse.getDirRecursive "."
                ]
            , bgroup
                "Test laziness"
                [ bench "unix-recursive String" $ nfIO (head . tail <$> String.followList ".")
                , bench "unix-recursive ByteString" $ nfIO (head . tail <$> ByteString.followList ".")
                , bench "dir-traverse" $ nfIO ((head . tail) <$> DirTraverse.getDirRecursive ".")
                ]
            ]
        ]
