{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module System.Metrics.SimpleExample
  (
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import System.Metrics
import qualified System.Metrics.Counter as Counter

-- A user-specified GADT statically determines the names, types, and
-- possible tags of the metrics that can be registered to the store.
data AppMetrics (name :: Symbol) (t :: MetricType) (tags :: Type) where
    RequestCount :: AppMetrics "myapp.request_count" 'CounterType ()

main = do
    store <- newStore @AppMetrics
    requests <- createCounter RequestCount () store
    -- Every time we receive a request:
    Counter.inc requests
