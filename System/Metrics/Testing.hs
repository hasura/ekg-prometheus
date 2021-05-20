{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Testing
  ( main
  ) where

import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Static

data MyMetrics (t :: MetricType) (name :: Symbol) (tags :: *) where
  Requests ::
    MyMetrics 'Counter "requests" EndpointTags
  DBConnections ::
    MyMetrics 'Gauge "postgres.total_connections" DataSourceTags

newtype EndpointTags = EndpointTags { endpointLabel :: T.Text }
  deriving (Generic)
instance ToTags EndpointTags

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
