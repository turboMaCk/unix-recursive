{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Data.ByteString.UTF8 as BS
import qualified System.Posix.Recursive.ByteString as Lib


main :: IO ()
main = do
    [path] <- getArgs
    !all <- Lib.listEverything $ BS.fromString path
    print $ length all
