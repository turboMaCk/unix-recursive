module Main where

import qualified Data.ByteString.UTF8 as BS
import System.Environment (getArgs)
import qualified System.Posix.Recursive.ByteString as Lib


main :: IO ()
main = do
    [path'] <- getArgs
    let path = BS.fromString path'
    all <- Lib.followList path
    print $ length all
