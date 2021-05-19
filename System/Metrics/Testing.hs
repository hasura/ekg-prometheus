{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Testing
  ( main
  ) where

import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import System.Metrics.Static
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

------------------------------------------------------------------------

data MyMetrics (t :: MetricType) (name :: Symbol) (tags :: *) where
  CrunchyRequests :: MyMetrics 'Counter "requests.crunchy" EndpointTags
  SpicyRequests :: MyMetrics 'Counter "requests.spicy" EndpointTags
  DBConnections :: MyMetrics 'Gauge "connections.db" DataSourceTags


newtype EndpointTags = EndpointTags { endpointLabel :: T.Text }
  deriving (Generic)

instance ToTags EndpointTags

data DataSourceTags = DataSourceTags
  { sourceName :: T.Text
  , connInfo :: T.Text
  }
  deriving (Generic)

instance ToTags DataSourceTags

------------------------------------------------------------------------

main :: IO ()
main = do
  store <- newStore
  crunchyRequstsCounter <-
    createCounter CrunchyRequests (EndpointTags "dev/harpsicord") store
  spicyRequstsCounter <-
    createCounter SpicyRequests (EndpointTags "dev/tabla") store
  dbConnectionsGauge <-
    let tags = DataSourceTags
          { sourceName = "testDB"
          , connInfo = "hasura.io/learn"
          }
    in  createGauge DBConnections tags store

  for_ [(1 :: Int) ..1000] $ \_ -> do
    Counter.inc crunchyRequstsCounter
    Counter.inc spicyRequstsCounter
    Gauge.inc dbConnectionsGauge
  stats <- sampleAll store
  for_ (M.toList stats) $ \(identifier, value) -> do
    print identifier
    print value
