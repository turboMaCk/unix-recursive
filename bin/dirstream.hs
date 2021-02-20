module Main where

import Control.Applicative
import Data.DirStream
import Data.String
import qualified Filesystem.Path as F
import Pipes
import Pipes.Prelude hiding (print)
import Pipes.Safe
import System.Environment
import Prelude hiding (length)


dirstreamGet :: F.FilePath -> ListT (SafeT IO) F.FilePath
dirstreamGet path = do
    child <- childOf path
    isDir <- liftIO $ isDirectory child
    if isDir
        then pure child <|> descendentOf child
        else pure child


main :: IO ()
main = do
    [path] <- getArgs

    count <-
        runSafeT $
            runEffect $
                (length (every (descendentOf (fromString path))))

    print count
