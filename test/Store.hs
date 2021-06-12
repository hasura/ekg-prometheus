{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Store
  ( tests
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import qualified Data.HashMap.Strict as HM
import Test.Hspec
import Test.HUnit (assertEqual)

import System.Metrics.Distribution.Internal
import System.Metrics.Internal.Store

tests :: Spec
tests =
  describe "The internal Store interface" $ do
    it "passes a smoke test" test_smokeTest

-- | A test that simply runs functions from the interface to make sure they
-- don't throw errors or never return, that is, that they don't evaluate to
-- bottom.
test_smokeTest :: IO ()
test_smokeTest = do
  result <- race (threadDelay 1000000) smokeTest
  assertEqual "Smoke test took too long" result (Right ())

smokeTest :: IO ()
smokeTest = do
  store <- newStore

  let counterIdentifier = Identifier "ccounter" mempty
      gaugeIdentifier = Identifier "cgauge" mempty
      labelIdentifier = Identifier "clabel" mempty
      distributionIdentifier = Identifier "cdistribution" mempty
  !_ <- createCounter counterIdentifier store
  !_ <- createCounter gaugeIdentifier store
  !_ <- createCounter labelIdentifier store
  !_ <- createCounter distributionIdentifier store

  deregistrationHandle <- register store $ mconcat
    [ registerCounter (Identifier "rcounter" mempty) (pure 0)
    , registerGauge (Identifier "rgauge" mempty) (pure 0)
    , registerLabel (Identifier "rlabel" mempty) (pure "")
    , registerDistribution (Identifier "rdistribution" mempty) (pure $ Stats 0 0 0 0 0 0)
    , flip registerGroup (pure ()) $ HM.fromList
        [ (Identifier "group" (HM.singleton "gcounter" mempty), const (Counter 0))
        , (Identifier "group" (HM.singleton "ggauge" mempty), const (Gauge 0))
        ]
    ]

  !_ <- sampleAll store

  deregister store $
    deregisterMetric counterIdentifier <>
    deregisterByName (idName distributionIdentifier)
  deregistrationHandle
