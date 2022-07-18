{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module System.Metrics.Prometheus.Example
  ( main
  ) where

import Data.Kind (Type)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import qualified System.Metrics.Prometheus.Counter as Counter
import qualified System.Metrics.Prometheus.Gauge as Gauge
import System.Metrics.Prometheus

-- Custom type describing a set of classes of metrics.
type MyMetrics :: Symbol -> Symbol -> MetricType -> Type -> Type
data MyMetrics name help metricType tags where
  Requests ::
    MyMetrics "requests" "" 'CounterType EndpointTags
  DBConnections ::
    MyMetrics "postgres.total_connections" "" 'GaugeType DataSourceTags

-- Custom tag set
newtype EndpointTags = EndpointTags { endpoint :: T.Text }
  deriving (Generic)
instance ToTags EndpointTags

-- Custom tag set
data DataSourceTags = DataSourceTags
  { sourceName :: T.Text
  , connInfo :: T.Text
  } deriving (Generic)
instance ToTags DataSourceTags

main :: IO ()
main = do
  store <- newStore
  harpsichordReqs <-
    createCounter Requests (EndpointTags "dev/harpsichord") store
  tablaReqs <-
    createCounter Requests (EndpointTags "dev/tabla") store
  dbConnections <-
    let tags = DataSourceTags
          { sourceName = "myDB"
          , connInfo = "localhost:5432"
          }
    in  createGauge DBConnections tags store

  Counter.add harpsichordReqs 5
  Counter.add tablaReqs 10
  Gauge.set dbConnections 99

  stats <- sampleAll store
  print stats
