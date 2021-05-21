{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Distribution
  ( tests
  ) where

import Data.Foldable (traverse_)
import System.Metrics
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Distribution.Internal as Distribution
import Test.Hspec
import Test.HUnit

tests :: Spec
tests =
  describe "The `Distribution` metric" $ do
    it "yields zero values when empty" test_empty
    it "computes its statistics correctly" test_stats

-- | Check that an distribution with no values returns zero for all its
-- statistics.
--
-- We return zero rather than @NaN@ or @Infinity@ in order to accomodate
-- downstream consumers that cannot parse such values as floats.
test_empty :: IO ()
test_empty = do
  dist <- Distribution.new
  stats <- Distribution.read dist

  assertBool "Mean of empty distribution not zero" $
    Distribution.mean stats == 0.0
  assertBool "Variance of empty distribution not zero" $
    Distribution.variance stats == 0.0
  assertBool "Count of empty distribution not zero" $
    Distribution.count stats == 0
  assertBool "Count of empty distribution not zero" $
    Distribution.sum stats == 0.0
  assertBool "Minimum of empty distribution not zero" $
    Distribution.min stats == 0.0
  assertBool "Maximum of empty distribution not zero" $
    Distribution.max stats == 0.0

test_stats :: IO ()
test_stats = do
  let sample = map (fromIntegral @Int) [1..10]
      sampleCount = length sample
      sampleSum = sum sample
      sampleMean = sampleSum / fromIntegral sampleCount
      sampleVariance =
        let sq x = x*x
            sumDiffSquares = sum $ map (sq . subtract sampleMean) sample
        in  sumDiffSquares / fromIntegral sampleCount

  dist <- Distribution.new
  traverse_ (Distribution.add dist) sample
  stats <- Distribution.read dist

  assertBool "Mean not correct" $
    Distribution.mean stats `approxEq` sampleMean
  assertBool "Variance not correct" $
    Distribution.variance stats `approxEq` sampleVariance
  assertBool "Count not correct" $
    Distribution.count stats == fromIntegral sampleCount
  assertBool "Sum not correct" $
    Distribution.sum stats `approxEq` sampleSum
  assertBool "Minimum not correct" $
    Distribution.min stats `approxEq` minimum sample
  assertBool "Maximum not correct" $
    Distribution.max stats `approxEq` maximum sample

approxEq :: Double -> Double -> Bool
approxEq x y = abs (x - y) < 1e-12
