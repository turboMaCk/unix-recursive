module Main where

import System.Environment
import qualified System.Posix.Files as Posix
import qualified System.Posix.Recursive as Lib


main :: IO ()
main = do
    [path] <- getArgs
    all <- Lib.followListEverything path
    print $ length all
