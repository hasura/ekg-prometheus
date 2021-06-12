module Main where

import Test.Hspec

import qualified Distribution
import qualified State
import qualified Store

main :: IO ()
main = hspec $ do
  State.tests
  Store.tests
  Distribution.tests
