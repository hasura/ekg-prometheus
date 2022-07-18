{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module System.Metrics.Prometheus.SimpleExample
  ( main
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.Counter as Counter

-- A user-specified GADT statically determines the names, types, and
-- possible tags of the metrics that can be registered to the store.
type AppMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type
data AppMetrics name help metricType tags where
    RequestCount :: AppMetrics "myapp.request_count" "" 'CounterType ()

main :: IO ()
main = do
    store <- newStore @AppMetrics
    requests <- createCounter RequestCount () store
    -- Every time we receive a request:
    Counter.inc requests
