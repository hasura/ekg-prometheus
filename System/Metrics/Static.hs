{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.Metrics.Static
  (
    -- * Static metric annotations
    MetricType (..)

    -- * The metric store
  , Store
  , newStore

    -- * Registering metrics
  , registerCounter
  , registerGauge

    -- ** Convenience functions
  , createCounter
  , createGauge

    -- ** Predefined metrics
  , registerGcMetrics

    -- * Deregistering metrics
  , deregisterByName

    -- * Sampling metrics
  , sampleAll
  ) where

import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge

------------------------------------------------------------------------
-- * Static metric annotations

data MetricType
  = Counter
  | Gauge

------------------------------------------------------------------------
-- * The metric store

newtype Store (f :: MetricType -> Symbol -> *) =
  Store { runStore :: Metrics.Store }

newStore :: IO (Store f)
newStore = Store <$> Metrics.newStore

------------------------------------------------------------------------
-- * Registering metrics

registerCounter ::
  forall f name.
  (KnownSymbol name) =>
  f 'Counter name ->
  M.HashMap T.Text T.Text ->
  IO Int64 ->
  Store f ->
  IO ()
registerCounter _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name tags
  in  Metrics.registerCounter identifier sample store

registerGauge ::
  forall f name.
  (KnownSymbol name) =>
  f 'Gauge name ->
  M.HashMap T.Text T.Text ->
  IO Int64 ->
  Store f ->
  IO ()
registerGauge _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name tags
  in  Metrics.registerGauge identifier sample store

------------------------------------------------------------------------
-- ** Convenience functions

createCounter ::
  forall f name.
  (KnownSymbol name) =>
  f 'Counter name ->
  M.HashMap T.Text T.Text ->
  Store f ->
  IO Counter.Counter
createCounter _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name tags
  in  Metrics.createCounter identifier store

createGauge ::
  forall f name.
  (KnownSymbol name) =>
  f 'Gauge name ->
  M.HashMap T.Text T.Text ->
  Store f ->
  IO Gauge.Gauge
createGauge _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name tags
  in  Metrics.createGauge identifier store

------------------------------------------------------------------------
-- ** Predefined metrics

registerGcMetrics :: Store f -> IO ()
registerGcMetrics (Store store) = Metrics.registerGcMetrics store

------------------------------------------------------------------------
-- * Deregistering metrics

deregisterByName :: T.Text -> Store f -> IO ()
deregisterByName name (Store store) =
  Metrics.deregisterByName name store

------------------------------------------------------------------------
-- * Sampling metrics

sampleAll :: Store f -> IO Metrics.Sample
sampleAll (Store store) = Metrics.sampleAll store
