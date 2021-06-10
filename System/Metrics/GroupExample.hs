{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module System.Metrics.GroupExample
  ( main
  ) where

import Data.Kind (Type)
import GHC.Stats
import GHC.TypeLits (Symbol)
import System.Metrics

data RTSMetrics (name :: Symbol) (t :: MetricType) (tags :: Type) where
  Gcs :: RTSMetrics "gcs" 'CounterType ()
  MaxLiveBytes :: RTSMetrics "max_live_bytes" 'GaugeType ()

main :: IO ()
main = do
  store <- newStore
  let samplingGroup =
        SamplingGroup
          :> (Gcs, (), fromIntegral . gcs)
          :> (MaxLiveBytes, (), fromIntegral . max_live_bytes)
  _ <- register store $ registerGroup samplingGroup getRTSStats
  pure ()
