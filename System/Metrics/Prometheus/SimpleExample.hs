{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module System.Metrics.Prometheus.SimpleExample
  ( main
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import System.Metrics.Prometheus
import qualified System.Metrics.Prometheus.Counter as Counter

-- A user-specified GADT statically determines the names, types, and
-- possible labels of the metrics that can be registered to the store.
data AppMetrics (name :: Symbol) (t :: MetricType) (labels :: Type) where
    RequestCount :: AppMetrics "myapp.request_count" 'CounterType ()

main :: IO ()
main = do
    store <- newStore @AppMetrics
    requests <- createCounter RequestCount () store
    -- Every time we receive a request:
    Counter.inc requests
