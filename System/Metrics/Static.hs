{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- This module wraps "System.Metrics" and presents an alternative
-- interface where the types, names, and tags of metrics registered to a
-- `Store` are statically known.
--
-- The functions presented in this interface are exactly the same as
-- their counterparts in "System.Metrics", except that they have been
-- restricted to work on only a narrow, user-defined set of inputs.
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
  , registerLabel
  , registerDistribution

    -- ** Convenience functions
  , createCounter
  , createGauge
  , createLabel
  , createDistribution

    -- ** Registering groups of metrics
  , GroupSampler (..)
  , registerGroup

    -- ** Predefined metrics
  , registerGcMetrics

    -- * Deregistering metrics
  , deregisterByName

    -- * Sampling metrics
  , sampleAll
  ) where

import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label

------------------------------------------------------------------------
-- * Static metric annotations

-- | Types of metrics.
data MetricType
  = Counter
  | Gauge
  | Label
  | Distribution

------------------------------------------------------------------------
-- * Tags

-- | Types that can be converted to sets of key-value pairs.
--
-- One may derive a `ToTags` instance for any record that exclusively
-- has fields of type `T.Text` via "GHC.Generics". For example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import qualified Data.Text as T
-- > import GHC.Generics
-- >
-- > data MyTags = MyTags
-- >   { key1 :: T.Text
-- >   , key2 :: T.Text
-- >   } deriving (Generic)
-- >
-- > instance ToTags MyTags
--
-- >>> toTags $ MyTags { key1 = "value1", key2 = "value2" }
-- fromList [("key1","value1"),("key2","value2")]
--
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
-- * Deriving `ToTags`
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

-- | A mutable metrics store, parameterized by a type @f@ whose values
-- @v@ represent the groups of metrics that may be registered to the
-- store.
--
-- The metrics of each group @v :: f metricType name tags@ have their
-- type, name, and tags determined by the type indices of @f@. For
-- example:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE DeriveGeneric #-}
-- > {-# LANGUAGE GADTs #-}
-- > {-# LANGUAGE KindSignatures #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main
-- >   ( main
-- >   ) where
-- >
-- > import qualified Data.Text as T
-- > import GHC.Generics
-- > import GHC.TypeLits
-- > import qualified System.Metrics.Counter as Counter
-- > import qualified System.Metrics.Gauge as Gauge
-- > import System.Metrics.Static
-- >
-- > data MyMetrics (t :: MetricType) (name :: Symbol) (tags :: *) where
-- >   Requests ::
-- >     MyMetrics 'Counter "requests" EndpointTags
-- >   DBConnections ::
-- >     MyMetrics 'Gauge "postgres.total_connections" DataSourceTags
-- >
-- > newtype EndpointTags = EndpointTags { endpointLabel :: T.Text }
-- >   deriving (Generic)
-- > instance ToTags EndpointTags
-- >
-- > data DataSourceTags = DataSourceTags
-- >   { sourceName :: T.Text
-- >   , connInfo :: T.Text
-- >   } deriving (Generic)
-- > instance ToTags DataSourceTags
-- >
-- > main :: IO ()
-- > main = do
-- >   store <- newStore
-- >   harpsichordReqs <-
-- >     createCounter Requests (EndpointTags "dev/harpsichord") store
-- >   tablaReqs <-
-- >     createCounter Requests (EndpointTags "dev/tabla") store
-- >   dbConnections <-
-- >     let tags = DataSourceTags
-- >           { sourceName = "myDB"
-- >           , connInfo = "localhost:5432"
-- >           }
-- >     in  createGauge DBConnections tags store
-- >
-- >   Counter.add harpsichordReqs 5
-- >   Counter.add tablaReqs 10
-- >   Gauge.set dbConnections 99
-- >   stats <- sampleAll store
-- >   print stats
--
-- >>> main
-- fromList
--  [ ( Identifier
--      { idName = "requests"
--      , idTags = fromList [("endpointLabel","dev/tabla")] }
--    , Counter 10 )
--  , ( Identifier
--      { idName = "postgres.total_connections"
--      , idTags = fromList [("sourceName","myDB"),("connInfo","localhost:5432")] }
--    , Gauge 99 )
--  , ( Identifier
--      { idName = "requests"
--      , idTags = fromList [("endpointLabel","dev/harpsichord")] }
--    , Counter 5 )
--  ]
newtype Store (f :: MetricType -> Symbol -> Type -> Type) =
  Store Metrics.Store

newStore :: IO (Store f)
newStore = Store <$> Metrics.newStore

------------------------------------------------------------------------
-- * Registering metrics

registerCounter
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Counter name tags
  -> tags
  -> IO Int64
  -> Store f
  -> IO ()
registerCounter _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerCounter identifier sample store

registerGauge
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Gauge name tags
  -> tags
  -> IO Int64
  -> Store f
  -> IO ()
registerGauge _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerGauge identifier sample store

registerLabel
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Label name tags
  -> tags
  -> IO T.Text
  -> Store f
  -> IO ()
registerLabel _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerLabel identifier sample store

registerDistribution
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Distribution name tags
  -> tags
  -> IO Distribution.Stats
  -> Store f
  -> IO ()
registerDistribution _ tags sample (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.registerDistribution identifier sample store

------------------------------------------------------------------------
-- ** Registering groups of metrics

type family MetricValue (t :: MetricType) :: Type where
  MetricValue 'Counter = Int64
  MetricValue 'Gauge = Int64
  MetricValue 'Label = T.Text
  MetricValue 'Distribution = Distribution.Stats

class ToMetricValue (t :: MetricType) where
  toMetricValue :: Proxy t -> MetricValue t -> Metrics.Value

instance ToMetricValue 'Counter      where toMetricValue _ = Metrics.Counter
instance ToMetricValue 'Gauge        where toMetricValue _ = Metrics.Gauge
instance ToMetricValue 'Label        where toMetricValue _ = Metrics.Label
instance ToMetricValue 'Distribution where toMetricValue _ = Metrics.Distribution


infixl 0 :>
data GroupSampler
  :: (MetricType -> Symbol -> Type -> Type) -> Type -> [Type] -> Type
  where
  GroupSampler :: IO env -> GroupSampler metrics env '[]
  (:>)
    :: GroupSampler metrics env xs
    ->  ( metrics metricType name tags
        , tags
        , env -> MetricValue metricType )
    -> GroupSampler metrics env (metrics metricType name tags ': xs)


class RegisterGroup (xs :: [Type]) where
  registerGroup_
    :: [(Metrics.Identifier, env -> Metrics.Value)]
    -> GroupSampler metrics env xs
    -> Store metrics
    -> IO ()

-- | Base case
instance RegisterGroup '[] where
  registerGroup_ getters (GroupSampler sampler) (Store store) =
    Metrics.registerGroup (M.fromList getters) sampler store

-- | Inductive case
instance
  ( RegisterGroup xs
  , ToMetricValue metricType
  , KnownSymbol name
  , ToTags tags
  ) => RegisterGroup (metrics metricType name tags ': xs)
  where
  registerGroup_ getters (groupSampler :> (_, tags, getter)) store =
    let identifier = Metrics.Identifier
          { Metrics.idName = T.pack $ symbolVal (Proxy @name)
          , Metrics.idTags = toTags tags }
        getter' =
          ( identifier
          , toMetricValue (Proxy @metricType) . getter )
    in  registerGroup_ (getter' : getters) groupSampler store

registerGroup
  :: (RegisterGroup xs)
  => GroupSampler metrics env xs -> Store metrics -> IO ()
registerGroup = registerGroup_ []

------------------------------------------------------------------------
-- ** Convenience functions

createCounter
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Counter name tags
  -> tags
  -> Store f
  -> IO Counter.Counter
createCounter _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.createCounter identifier store

createGauge
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Gauge name tags
  -> tags
  -> Store f
  -> IO Gauge.Gauge
createGauge _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.createGauge identifier store

createLabel
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Gauge name tags
  -> tags
  -> Store f
  -> IO Label.Label
createLabel _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.createLabel identifier store

createDistribution
  :: forall f name tags. (KnownSymbol name, ToTags tags)
  => f 'Gauge name tags
  -> tags
  -> Store f
  -> IO Distribution.Distribution
createDistribution _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Metrics.createDistribution identifier store

------------------------------------------------------------------------
-- ** Predefined metrics

registerGcMetrics :: Store f -> IO ()
registerGcMetrics (Store store) = Metrics.registerGcMetrics store

------------------------------------------------------------------------
-- * Deregistering metrics

deregisterByName
  :: forall f metricType name tags. (KnownSymbol name)
  => f metricType name tags
  -> Store f
  -> IO ()
deregisterByName _ (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
  in  Metrics.deregisterByName name store

------------------------------------------------------------------------
-- * Sampling metrics

sampleAll :: Store f -> IO Metrics.Sample
sampleAll (Store store) = Metrics.sampleAll store
