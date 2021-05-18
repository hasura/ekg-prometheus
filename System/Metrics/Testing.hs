{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module System.Metrics.Testing
  ( main
  ) where

import Data.Foldable (for_)
import qualified Data.HashMap.Strict as M
import GHC.TypeLits
import System.Metrics.Static
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

------------------------------------------------------------------------

data MyMetrics (t :: MetricType) (name :: Symbol) where
  CrunchyRequests :: MyMetrics 'Counter "requests.crunchy"
  SpicyRequests :: MyMetrics 'Counter "requests.spicy"
  DBConnections :: MyMetrics 'Gauge "connections.db"

main :: IO ()
main = do
  store <- newStore
  crunchyRequstsCounter <- createCounter CrunchyRequests mempty store
  spicyRequstsCounter <- createCounter SpicyRequests mempty store
  dbConnectionsGauge <- createGauge DBConnections mempty store
  for_ [(1 :: Int) ..1000] $ \_ -> do
    Counter.inc crunchyRequstsCounter
    Counter.inc spicyRequstsCounter
    Gauge.inc dbConnectionsGauge
  stats <- sampleAll store
  for_ (M.toList stats) $ \(identifier, value) -> do
    print identifier
    print value
