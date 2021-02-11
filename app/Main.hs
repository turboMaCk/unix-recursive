{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import qualified Data.ByteString.UTF8 as BS
import qualified System.Posix.Recursive as Lib


main :: IO ()
main = do
    [path] <- getArgs
    !all <- Lib.list $ BS.fromString path
    print $ length all
