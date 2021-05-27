{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module System.Metrics.Static.GroupExample
  ( main
  ) where

import Data.Kind (Type)
import GHC.Stats
import GHC.TypeLits
import System.Metrics.Static

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
  registerGroup samplingGroup getRTSStats store
