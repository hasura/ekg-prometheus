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
  RTSGcs :: RTSMetrics "gcs" 'CounterType ()
  RTSMaxLiveBytes :: RTSMetrics "max_live_bytes" 'GaugeType ()

main :: IO ()
main = do
  store <- newStore
  let samplingGroup =
        SamplingGroup
          :> (RTSGcs, (), fromIntegral . gcs)
          :> (RTSMaxLiveBytes, (), fromIntegral . max_live_bytes)
  _ <- register store $ registerGroup samplingGroup getRTSStats
  pure ()
