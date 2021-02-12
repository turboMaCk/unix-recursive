{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Directory.Recursive as Lib
import System.Environment


main :: IO ()
main = do
    [path] <- getArgs
    !all <- Lib.getDirRecursive path
    print $ length all
