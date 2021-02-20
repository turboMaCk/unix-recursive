module Main where

import System.Environment (getArgs)
import qualified System.Posix.Recursive as Lib


main :: IO ()
main = do
    [path] <- getArgs
    all <- Lib.followListEverything path
    print $ length all
