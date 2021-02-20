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
            "normal form"
            [ bench "unix-recursive String" $ nfIO $ String.listEverything "."
            , bench "unix-recursive String dirs only" $ nfIO $ String.listDirectories "."
            , bench "unix-recursive ByteString" $ nfIO $ ByteString.listEverything "."
            , bench "unix-recursive ByteString dirs only" $ nfIO $ ByteString.listDirectories "."
            , bench "dir-traverse" $ nfIO $ DirTraverse.getDirRecursive "."
            ]
        , bgroup
            "just head"
            [ bench "unix-recursive String" $ nfIO (head <$> String.listEverything ".")
            , bench "unix-recursive String dirs only" $ nfIO (head <$> String.listDirectories ".")
            , bench "unix-recursive ByteString" $ nfIO (head <$> ByteString.listEverything ".")
            , bench "unix-recursive ByteString dirs only" $ nfIO (head <$> ByteString.listDirectories ".")
            , bench "dir-traverse" $ nfIO (head <$> DirTraverse.getDirRecursive ".")
            ]
        ]
