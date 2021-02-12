{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Test.Hspec (hspec)
import Spec.ByteString as ByteString


main :: IO ()
main =
    hspec $ ByteString.spec
