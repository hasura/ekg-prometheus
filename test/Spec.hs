module Main where

import Test.Hspec

import qualified Counter
import qualified State
import qualified Store

main :: IO ()
main = hspec $ do
  State.tests
  Store.tests
  Counter.tests
