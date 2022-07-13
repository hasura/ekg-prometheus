{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Export
  ( tests
  ) where

import qualified Data.ByteString.Builder as BB
import qualified Data.HashMap.Strict as M
import Data.Foldable (for_)
import Data.Kind (Type)
import qualified Data.Text as T
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.Export (sampleToPrometheus)
import qualified System.Metrics.Prometheus.Gauge as Gauge
import qualified System.Metrics.Prometheus.Histogram as Histogram
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

-- | Test whether the output of 'sampleToPrometheus is consistent with its
-- Haddocks.
tests :: Spec
tests =
  describe "The output of `sampleToPrometheus`" $
    it "is consistent with its haddocks" $ do

  store <- newStore @ExampleMetrics

  gauge <- createGauge ExampleGauge () store
  Gauge.set gauge 100

  counter1 <-
    createCounter
      ExampleCounter
      (M.fromList [("tag.name.1", "tag value 1"), ("tag.name.2", "tag value 1")])
      store
  Counter.add counter1 10

  counter2 <-
    createCounter
      ExampleCounter
      (M.fromList [("tag.name.1", "tag value 2"), ("tag.name.2", "tag value 2")])
      store
  Counter.add counter2 11

  let upperBounds = [1, 2, 3]
  histogram <-
    createHistogram
      upperBounds
      ExampleHistogram
      (M.singleton "tag" "value")
      store
  for_ (4:upperBounds) (Histogram.observe histogram)

  prometheusSample <- BB.toLazyByteString . sampleToPrometheus <$> sampleAll store

  shouldBe prometheusSample
    "# TYPE _100gauge gauge\n_100gauge 100.0\n\n# TYPE my_counter counter\nmy_counter{tag_name_1=\"tag value 1\",tag_name_2=\"tag value 1\"} 10.0\nmy_counter{tag_name_1=\"tag value 2\",tag_name_2=\"tag value 2\"} 11.0\n\n# TYPE my_histogram histogram\nmy_histogram_bucket{tag=\"value\",le=\"1.0\"} 1.0\nmy_histogram_bucket{tag=\"value\",le=\"2.0\"} 2.0\nmy_histogram_bucket{tag=\"value\",le=\"3.0\"} 3.0\nmy_histogram_bucket{tag=\"value\",le=\"+Inf\"} 4.0\nmy_histogram_sum{tag=\"value\"} 10.0\nmy_histogram_count{tag=\"value\"} 4.0\n"

data ExampleMetrics :: Symbol -> MetricType -> Type -> Type where
  ExampleGauge
    :: ExampleMetrics "100gauge" 'GaugeType ()
  ExampleCounter
    :: ExampleMetrics "my.counter" 'CounterType (M.HashMap T.Text T.Text)
  ExampleHistogram
    :: ExampleMetrics "my.histogram" 'HistogramType (M.HashMap T.Text T.Text)
