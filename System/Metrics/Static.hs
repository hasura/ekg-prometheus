{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module System.Metrics.Static
  (
    -- * Static metric annotations
    MetricType (..)
  , ToTags (..)

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
import GHC.Generics
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
-- * Tags

class ToTags a where
  toTags :: a -> M.HashMap T.Text T.Text

  default toTags ::
    (Generic a, GToTags (Rep a)) => a -> M.HashMap T.Text T.Text
  toTags x = gToTags "" (from x)
  {-# INLINE toTags #-}

instance ToTags () where
  toTags () = M.empty
  {-# INLINE toTags #-}

------------------------------------------------------------------------
-- * Deriving ToTags
--
-- Deriving instances of `ToTags` for records that exclusively have
-- fields of type `Text`.

class GToTags (f :: * -> *) where
  gToTags :: T.Text -> f x -> M.HashMap T.Text T.Text

-- Data (passthrough)
instance (GToTags f) => GToTags (D1 c f) where
  gToTags name (M1 x) = gToTags name x
  {-# INLINE gToTags #-}

-- Constructor (passthrough)
instance (GToTags f) => GToTags (C1 c f) where
  gToTags name (M1 x) = gToTags name x
  {-# INLINE gToTags #-}

-- Products (union)
instance (GToTags f, GToTags g) => GToTags (f :*: g) where
  gToTags name (x :*: y) = gToTags name x `M.union` gToTags name y
  {-# INLINE gToTags #-}

-- Record selectors (take record selector name)
instance (GToTags f, KnownSymbol name) =>
  GToTags (S1 ('MetaSel ('Just name) su ss ds) f) where
  gToTags _name (M1 x) =
    let name' = T.pack $ symbolVal $ Proxy @name
    in  gToTags name' x
  {-# INLINE gToTags #-}

-- Individual fields (take value, combine with name)
instance GToTags (K1 i T.Text) where
  gToTags name (K1 x) = M.singleton name x
  {-# INLINE gToTags #-}

------------------------------------------------------------------------
-- * The metric store

newtype Store (f :: MetricType -> Symbol -> * -> *) =
  Store { runStore :: Metrics.Store }

newStore :: IO (Store f)
newStore = Store <$> Metrics.newStore

------------------------------------------------------------------------
-- * Registering metrics

registerCounter ::
  forall f name tags.
  (KnownSymbol name, ToTags tags) =>
  f 'Counter name tags ->
  tags ->
  IO Int64 ->
  Store f ->
  IO ()
registerCounter _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerCounter identifier sample store

registerGauge ::
  forall f name tags.
  (KnownSymbol name, ToTags tags) =>
  f 'Gauge name tags ->
  tags ->
  IO Int64 ->
  Store f ->
  IO ()
registerGauge _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerGauge identifier sample store

------------------------------------------------------------------------
-- ** Convenience functions

createCounter ::
  forall f name tags.
  (KnownSymbol name, ToTags tags) =>
  f 'Counter name tags ->
  tags ->
  Store f ->
  IO Counter.Counter
createCounter _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.createCounter identifier store

createGauge ::
  forall f name tags.
  (KnownSymbol name, ToTags tags) =>
  f 'Gauge name tags ->
  tags ->
  Store f ->
  IO Gauge.Gauge
createGauge _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
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
