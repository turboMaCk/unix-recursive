{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Spec.ByteString as ByteString
import Test.Hspec (hspec)


main :: IO ()
main =
    hspec $ ByteString.spec
