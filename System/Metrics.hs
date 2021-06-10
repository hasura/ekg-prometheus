{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- A module for defining metrics that can be monitored.
--
-- Metrics are used to monitor program behavior and performance. All
-- metrics have
--
--  * a name,
--
--  * a set of tags, and
--
--  * a way to get the metric's current value.
--
-- This module provides a way to register metrics in a global \"metric
-- store\". The store can then be used to get a snapshot of all
-- metrics. The store also serves as a central place to keep track of
-- all the program's metrics, both user and library defined.
--
-- Here's an example of creating a single counter, used to count the
-- number of request served by a web server:
--
-- > import Data.Kind (Type)
-- > import GHC.TypeLits (Symbol)
-- > import System.Metrics
-- > import qualified System.Metrics.Counter as Counter
-- >
-- > -- A user-specified GADT statically determines the names, types, and
-- > -- possible tags of the metrics that can be registered to the store.
-- >
-- > data AppMetrics (name :: Symbol) (t :: MetricType) (tags :: Type) where
-- >     RequestCount :: AppMetrics "myapp.request_count" 'CounterType ()
-- >
-- > main = do
-- >     store <- newStore @AppMetrics
-- >     requests <- createCounter RequestCount () store
-- >     -- Every time we receive a request:
-- >     Counter.inc requests
--
-- This module also provides a way to register a number of predefined
-- metrics that are useful in most applications. See e.g.
-- 'registerGcMetrics'.


-- Implementation note:
-- This module merely wraps and restricts the interface of
-- "System.Metrics.Internal.Store". That is, the functions presented in
-- this interface are exactly the same as their counterparts in
-- "System.Metrics", except that they have been restricted to work on
-- only a narrow, user-defined set of inputs.

module System.Metrics
  (
    -- * Naming metrics
    -- $naming

    -- * The metric store
    -- $metric-store
    Store
  , newStore

    -- * Static metric annotations
  , MetricType (..)

    -- ** Tags
  , ToTags (..)

    -- * Changing scope
    -- $scopes
  , subset

    -- ** Common scopes
  , EmptyMetrics
  , emptyOf
  , AllMetrics (..)
  , ofAll

    -- * Registering and deregistering metrics
    -- $registering-and-deregistering

    -- ** Registering
    -- $registering
  , register
  , Registration
  , registerCounter
  , registerGauge
  , registerLabel
  , registerDistribution
  , registerGroup
  , SamplingGroup (..)
  , MetricValue

    -- ** Convenience functions
    -- $convenience
  , createCounter
  , createGauge
  , createLabel
  , createDistribution

    -- ** Deregistering
    -- $deregistering
  , Deregistration
  , deregister
  , deregisterMetric
  , deregisterClass

    -- * Sampling metrics
    -- $sampling
  , sampleAll
  , Metrics.Sample
  , Metrics.Identifier (..)
  , Metrics.Value (..)

    -- * Predefined metrics
    -- $predefined
  , registerGcMetrics
  , GcMetrics (..)
  ) where

import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import qualified GHC.Stats as Stats
import GHC.TypeLits
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Internal.Store as Metrics -- TODO: Import as Internal
import qualified System.Metrics.Label as Label

-- $naming
-- Compound metric names should be separated using underscores.
-- Example: @request_count@. Periods in the name imply namespacing.
-- Example: @\"myapp.users\"@. Some consumers of metrics will use
-- these namespaces to group metrics in e.g. UIs.
--
-- Libraries and frameworks that want to register their own metrics
-- should prefix them with a namespace, to avoid collision with
-- user-defined metrics and metrics defined by other libraries. For
-- example, the Snap web framework could prefix all its metrics with
-- @\"snap.\"@.
--
-- It's customary to suffix the metric name with a short string
-- explaining the metric's type e.g. using @\"_ms\"@ to denote
-- milliseconds.

------------------------------------------------------------------------
-- * The metric store

-- $metric-store
-- The metric store is a shared store of metrics. It allows several
-- disjoint components (e.g. libraries) to contribute to the set of
-- metrics exposed by an application. Libraries that want to provide a
-- set of metrics should define a register method, in the style of
-- 'registerGcMetrics', that registers the metrics in the 'Store'. The
-- register function should document which metrics are registered and
-- their types (i.e. counter, gauge, label, or distribution).
--
-- References to metric stores are parameterized a type that restricts
-- the kinds of metrics that may be added to or removed from the `Store`
-- through the reference. In other words, they are /scoped/ by their
-- type parameter. Users are expected to parameterize their metric
-- stores with custom GADTs that describe the kinds of metrics that may
-- be collected in their applications.

-- | A mutable metric store, parameterized by a type @metrics@ whose
-- values @v@ represent the classes of metrics that may be registered to
-- the store.
--
-- The metrics of each class @v :: metrics name metricType tags@ have
-- their name, metric type, and possible tags statically determined by
-- the respective type indices of @metrics@.
newtype Store (metrics :: Symbol -> MetricType -> Type -> Type) =
  Store Metrics.Store

-- | Create a new, empty metric store.
newStore :: IO (Store metrics)
newStore = Store <$> Metrics.newStore

------------------------------------------------------------------------
-- * Static metric annotations

-- | An enumeration of the types of metrics. To be used as types/kinds
-- via -XDataKinds.
data MetricType
  = CounterType
  | GaugeType
  | LabelType
  | DistributionType

-- | The type of values sampled by each metric.
type family MetricValue (t :: MetricType) :: Type where
  MetricValue 'CounterType = Int64
  MetricValue 'GaugeType = Int64
  MetricValue 'LabelType = T.Text
  MetricValue 'DistributionType = Distribution.Stats

-- | The `Metrics.Value` constructor for each metric.
class ToMetricValue (t :: MetricType) where
  toMetricValue :: Proxy t -> MetricValue t -> Metrics.Value

instance ToMetricValue 'CounterType      where toMetricValue _ = Metrics.Counter
instance ToMetricValue 'GaugeType        where toMetricValue _ = Metrics.Gauge
instance ToMetricValue 'LabelType        where toMetricValue _ = Metrics.Label
instance ToMetricValue 'DistributionType where toMetricValue _ = Metrics.Distribution

-- | The default implementation of each metric.
type family MetricsImpl (t :: MetricType) where
  MetricsImpl 'CounterType = Counter.Counter
  MetricsImpl 'GaugeType = Gauge.Gauge
  MetricsImpl 'LabelType = Label.Label
  MetricsImpl 'DistributionType = Distribution.Distribution

------------------------------------------------------------------------
-- ** Tags

-- | A class of types that can be converted to sets of key-value pairs
-- ("tags"), which are used to annotate metrics with metadata.
--
-- Each metric must be associated with a type from this typeclass. The
-- type determines the possible tag sets that may be attached to the
-- metric.
--
-- For convenience, one may derive, via "GHC.Generics", a `ToTags`
-- instance for any record that exclusively has fields of type `T.Text`.
-- [TODO: Support more field types for deriving.]
--
-- Example usage:
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
  toTags x = gToTags undefined (from x)
  {-# INLINE toTags #-}

-- | Disallow tags altogether.
--
-- > toTags () = HashMap.empty
instance ToTags () where
  toTags () = M.empty
  {-# INLINE toTags #-}

-- | Place no constraints on tags.
--
-- > toTags @(HashMap Text Text) = id
instance ToTags (M.HashMap T.Text T.Text) where
  toTags = id
  {-# INLINE toTags #-}

------------------------------------------------------------------------
-- ** Deriving `ToTags`
--
-- | Deriving instances of `ToTags` for records that exclusively have
-- fields of type `Text`.
class GToTags (f :: Type -> Type) where
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
-- * Changing scope

-- $scopes
-- References to metric stores are parameterized by types that restrict
-- the kinds of metrics that may be added to or removed from the store
-- through the references. In other words, they are /scoped/ by their
-- type parameter.
--
-- It may be useful to create a new reference to a metric store that is
-- scoped to a subset of its metrics. This can be done as long as the
-- subset can be represented by a function (see `subset`).

-- | Create a new reference to a metric store with restricted scope.
subset
  :: (forall name metricType tags.
      metricsSubset name metricType tags -> metrics name metricType tags)
    -- ^ Subset
  -> Store metrics -- ^ Reference
  -> Store metricsSubset -- ^ Restricted reference
subset _ = coerce

-- ** Common scopes

-- | The smallest scope, containing no metrics. This scope can be
-- embedded in any scope via the `emptyOf` function.
--
-- The only operation available to a store with scope @`EmptyMetrics`@
-- is `sampleAll`.
data EmptyMetrics :: Symbol -> MetricType -> Type -> Type where

-- | The smallest scope can be embedded in any scope.
emptyOf
  :: EmptyMetrics name metricType tags -> metrics name metricType tags
emptyOf metrics = case metrics of {}

-- | The largest scope, containing all metrics. All scopes can be
-- embedded in this scope via the `ofAll` function.
--
-- Metrics of any form may be registered to a store of type `AllMetrics`
-- using the `Metric` constructor. For example:
--
-- > example :: Store AllMetrics -> IO Counter.Counter
-- > example = createCounter (Metrics @"total_requests") ()
--
data AllMetrics :: Symbol -> MetricType -> Type -> Type where
  Metric :: AllMetrics name metricType tags

_exampleAllMetrics :: Store AllMetrics -> IO Counter.Counter
_exampleAllMetrics = createCounter (Metric @"total_requests") ()

-- | All scopes can be embedded in the largest scope.
ofAll
  :: metrics name metricType tags -> AllMetrics name metricType tags
ofAll _ = Metric

------------------------------------------------------------------------
-- * Registering and deregistering metrics

-- $registering-and-deregistering
-- Before metrics can be sampled, they need to be registered with the
-- metric store. Once registered, metrics can also be deregistered.

------------------------------------------------------------------------
-- ** Registering

-- $registering
-- Metrics are identified by the combination of the class they are
-- registered to and the tag set they are registered with. If you try to
-- register a metric at an identifier that is already in use by an
-- existing metric, the existing metric will be deregistered and
-- replaced by the new metric.
--
-- Upon `register`ing a set of metrics, you will be given a handle that
-- can be used to deregister the newly registered metrics /specifically/,
-- in the following sense. If a deregistration handle is supposed to
-- deregister a metric, and that metric is replaced by a new metric, the
-- new metric will not be deregistered if the handle is handle used.

-- | Atomically register one or more metrics. Returns a handle for
-- (atomically) deregistering those metrics specifically.
register
  :: Store metrics -- ^ Metric store
  -> Registration metrics -- ^ Registration action
  -> IO (IO ()) -- ^ Deregistration handle
register (Store store) (Registration registration) =
    Metrics.register store registration

-- | An action that registers one or more metrics to a metric store.
-- Can only be run by `register`.
newtype Registration (metric :: Symbol -> MetricType -> Type -> Type)
  = Registration Metrics.Registration

-- | Combine registration actions by running one after the other.
deriving instance Semigroup (Registration metrics)

deriving instance Monoid (Registration metrics)

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'CounterType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> IO Int64 -- ^ Action to read the current metric value
  -> Registration metrics
registerCounter = registerGeneric Metrics.registerCounter

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'GaugeType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> IO Int64 -- ^ Action to read the current metric value
  -> Registration metrics
registerGauge = registerGeneric Metrics.registerGauge

-- | Register a text metric. The provided action to read the value
-- must be thread-safe. Also see 'createLabel'.
registerLabel
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'LabelType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> IO T.Text -- ^ Action to read the current metric value
  -> Registration metrics
registerLabel = registerGeneric Metrics.registerLabel

-- | Register a distribution metric. The provided action to read the
-- value must be thread-safe. Also see 'createDistribution'.
registerDistribution
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'DistributionType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> IO Distribution.Stats -- ^ Action to read the current metric value
  -> Registration metrics
registerDistribution = registerGeneric Metrics.registerDistribution

registerGeneric
  :: forall metrics name metricType tags. (KnownSymbol name, ToTags tags)
  => ( Metrics.Identifier
      -> IO (MetricValue metricType)
      -> Metrics.Registration)
  -> metrics name metricType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> IO (MetricValue metricType) -- ^ Action to read the current metric value
  -> Registration metrics -- ^ Registration action
registerGeneric f _ tags sample =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Registration $ f identifier sample

-- | Register an action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in \"bulk\", as the run-time system provides a function to sample all
-- GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the provided
-- action computes @a@ atomically (e.g. if @a@ is a record, the action
-- needs to compute its fields atomically if the sampling is to be
-- atomic.)
--
-- Example usage:
--
-- > import Data.Kind (Type)
-- > import GHC.Stats
-- > import GHC.TypeLits (Symbol)
-- > import System.Metrics
-- >
-- > data RTSMetrics (name :: Symbol) (t :: MetricType) (tags :: Type) where
-- >   Gcs :: RTSMetrics "gcs" 'CounterType ()
-- >   MaxLiveBytes :: RTSMetrics "max_live_bytes" 'GaugeType ()
-- >
-- > main = do
-- >   store <- newStore
-- >   let samplingGroup =
-- >         SamplingGroup
-- >           :> (Gcs, (), fromIntegral . gcs)
-- >           :> (MaxLiveBytes, (), fromIntegral . max_live_bytes)
-- >   _ <- register store $ registerGroup samplingGroup getRTSStats
-- >   pure ()
--
-- (Note: The @RegisterGroup@ constraint can safely be ignored.)
--
registerGroup
  :: (RegisterGroup xs)
  => SamplingGroup metrics env xs -- ^ Metric identifiers and getter functions
  -> IO env -- ^ Action to sample the metric group
  -> Registration metrics -- ^ Registration action
registerGroup = registerGroup_ []


infixl 9 :>
-- | A group of metrics derived from the same sample.
data SamplingGroup
  :: (Symbol -> MetricType -> Type -> Type)
  -> Type
  -> [Type]
  -> Type
  where
  -- | The empty sampling group
  SamplingGroup :: SamplingGroup metrics env '[]
  -- | Add a metric to a sampling group
  (:>)
    :: SamplingGroup metrics env xs -- ^ Group to add to
    ->  ( metrics name metricType tags
        , tags
        , env -> MetricValue metricType )
        -- ^ Metric class, Tags, Getter function
    -> SamplingGroup metrics env (metrics name metricType tags ': xs)


-- | Helper class for `registerGroup`.
class RegisterGroup (xs :: [Type]) where
  registerGroup_
    :: [(Metrics.Identifier, env -> Metrics.Value)] -- ^ Processed metrics
    -> SamplingGroup metrics env xs -- ^ Metrics to be processed
    -> IO env -- ^ Action to sample the metric group
    -> Registration metrics

-- | Base case
instance RegisterGroup '[] where
  registerGroup_ getters SamplingGroup sample =
    Registration $ Metrics.registerGroup (M.fromList getters) sample

-- | Inductive case
instance
  ( RegisterGroup xs
  , ToMetricValue metricType
  , KnownSymbol name
  , ToTags tags
  ) => RegisterGroup (metrics name metricType tags ': xs)
  where
  registerGroup_ getters (group :> (_, tags, getter)) sample =
    let identifier = Metrics.Identifier
          { Metrics.idName = T.pack $ symbolVal (Proxy @name)
          , Metrics.idTags = toTags tags }
        getter' =
          ( identifier
          , toMetricValue (Proxy @metricType) . getter )
    in  registerGroup_ (getter' : getters) group sample

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a `System.Metrics.Counter.Counter`) with registering that reference
-- in the store in one convenient function. The deregistration handles
-- are discarded.

-- | Create and register a zero-initialized counter.
createCounter
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'CounterType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Store metrics -- ^ Metric store
  -> IO Counter.Counter
createCounter = createGeneric Metrics.createCounter

-- | Create and register a zero-initialized gauge.
createGauge
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'GaugeType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Store metrics -- ^ Metric store
  -> IO Gauge.Gauge
createGauge = createGeneric Metrics.createGauge

-- | Create and register an empty label.
createLabel
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'LabelType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Store metrics -- ^ Metric store
  -> IO Label.Label
createLabel = createGeneric Metrics.createLabel

-- | Create and register an event tracker.
createDistribution
  :: forall metrics name tags. (KnownSymbol name, ToTags tags)
  => metrics name 'DistributionType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Store metrics -- ^ Metric store
  -> IO Distribution.Distribution
createDistribution = createGeneric Metrics.createDistribution

createGeneric
  :: forall metrics name metricType tags. (KnownSymbol name, ToTags tags)
  => (Metrics.Identifier -> Metrics.Store -> IO (MetricsImpl metricType))
  -> metrics name metricType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Store metrics -- ^ Metric store
  -> IO (MetricsImpl metricType)
createGeneric f _ tags (Store store) =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  f identifier store

------------------------------------------------------------------------
-- ** Deregistering

-- $deregistering
-- Deregistration handles are the safest way to deregister metrics,
-- but you can also deregister metrics with this alternative interface.

-- | Atomically apply a deregistration action to a metrics store.
deregister
  :: Store metrics -- ^ Metric store
  -> Deregistration metrics -- ^ Deregistration action
  -> IO ()
deregister (Store store) (Deregistration deregistration) =
    Metrics.deregister store deregistration

-- | An action that deregisters one or more metrics from a metric store.
-- Can only be run by `deregister`.
newtype Deregistration (metrics :: Symbol -> MetricType -> Type -> Type)
  = Deregistration Metrics.Deregistration

-- | Combine deregistration actions by running one after the other.
deriving instance Semigroup (Deregistration metrics)

deriving instance Monoid (Deregistration metrics)

-- | Deregister a metric with a specific class and tag set.
deregisterMetric
  :: forall metrics name metricType tags.
      (KnownSymbol name, ToTags tags)
  => metrics name metricType tags -- ^ Metric class
  -> tags -- ^ Tags
  -> Deregistration metrics
deregisterMetric _ tags =
  let name = T.pack $ symbolVal (Proxy @name)
      identifier = Metrics.Identifier name (toTags tags)
  in  Deregistration $ Metrics.deregisterMetric identifier

-- | Deregister all the metrics registered to a class.
deregisterClass
  :: forall metrics name metricType tags. (KnownSymbol name)
  => metrics name metricType tags -- ^ Metric class
  -> Deregistration metrics
deregisterClass _ =
  let name = T.pack $ symbolVal (Proxy @name)
  in  Deregistration $ Metrics.deregisterByName name

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store metrics -> IO Metrics.Sample
sampleAll (Store store) = Metrics.sampleAll store

------------------------------------------------------------------------
-- * Predefined metrics

-- | Convert nanoseconds to milliseconds.
nsToMs :: Int64 -> Int64
nsToMs s = round (realToFrac s / (1000000.0 :: Double))

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- To enable GC statistics collection, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.
--
registerGcMetrics :: Registration GcMetrics
registerGcMetrics = registerGroup samplingGroup getRTSStats
  where
  samplingGroup = SamplingGroup
    :> (BytesAllocated, (), fromIntegral . Stats.allocated_bytes)
    :> (NumGcs, (), fromIntegral . Stats.gcs)
    :> (NumBytesUsageSamples, (), fromIntegral . Stats.major_gcs)
    :> (CumulativeBytesUsed, (), fromIntegral . Stats.cumulative_live_bytes)
    :> (BytesCopied, (), fromIntegral . Stats.copied_bytes)
#if MIN_VERSION_base(4,12,0)
    :> (InitCpuMs, (), nsToMs . Stats.init_cpu_ns)
    :> (InitWallMs, (), nsToMs . Stats.init_elapsed_ns)
#endif
    :> (MutatorCpuMs, (), nsToMs . Stats.mutator_cpu_ns)
    :> (MutatorWallMs, (), nsToMs . Stats.mutator_elapsed_ns)
    :> (GcCpuMs, (), nsToMs . Stats.gc_cpu_ns)
    :> (GcWallMs, (), nsToMs . Stats.gc_elapsed_ns)
    :> (CpuMs, (), nsToMs . Stats.cpu_ns)
    :> (WallMs, (), nsToMs . Stats.elapsed_ns)
    :> (MaxBytesUsed, (), fromIntegral . Stats.max_live_bytes)
    :> (CurrentBytesUsed, (), fromIntegral . Stats.gcdetails_live_bytes . Stats.gc)
    :> (CurrentBytesSlop, (), fromIntegral . Stats.gcdetails_slop_bytes . Stats.gc)
    :> (MaxBytesSlop, (), fromIntegral . Stats.max_slop_bytes)
    :> (PeakMegabytesAllocated, (), fromIntegral . (`quot` (1024*1024)) . Stats.max_mem_in_use_bytes)
    :> (ParTotBytesCopied, (), fromIntegral . Stats.par_copied_bytes)
    :> (ParMaxBytesCopied, (), fromIntegral . Stats.cumulative_par_max_copied_bytes)

-- | Get RTS statistics.
getRTSStats :: IO Stats.RTSStats
getRTSStats = do
    enabled <- Stats.getRTSStatsEnabled
    if enabled
        then Stats.getRTSStats
        else return emptyRTSStats

-- | Empty RTS statistics, as if the application hasn't started yet.
emptyRTSStats :: Stats.RTSStats
emptyRTSStats = Stats.RTSStats
    { gcs                                  = 0
    , major_gcs                            = 0
    , allocated_bytes                      = 0
    , max_live_bytes                       = 0
    , max_large_objects_bytes              = 0
    , max_compact_bytes                    = 0
    , max_slop_bytes                       = 0
    , max_mem_in_use_bytes                 = 0
    , cumulative_live_bytes                = 0
    , copied_bytes                         = 0
    , par_copied_bytes                     = 0
    , cumulative_par_max_copied_bytes      = 0
#if MIN_VERSION_base(4,11,0)
    , cumulative_par_balanced_copied_bytes = 0
#if MIN_VERSION_base(4,12,0)
    , init_cpu_ns                          = 0
    , init_elapsed_ns                      = 0
#endif
#endif
    , mutator_cpu_ns                       = 0
    , mutator_elapsed_ns                   = 0
    , gc_cpu_ns                            = 0
    , gc_elapsed_ns                        = 0
    , cpu_ns                               = 0
    , elapsed_ns                           = 0
#if MIN_VERSION_base(4,14,1)
    , nonmoving_gc_sync_cpu_ns             = 0
    , nonmoving_gc_sync_elapsed_ns         = 0
    , nonmoving_gc_sync_max_elapsed_ns     = 0
    , nonmoving_gc_cpu_ns                  = 0
    , nonmoving_gc_elapsed_ns              = 0
    , nonmoving_gc_max_elapsed_ns          = 0
#endif
    , gc                                   = emptyGCDetails
    }

emptyGCDetails :: Stats.GCDetails
emptyGCDetails = Stats.GCDetails
    { gcdetails_gen                       = 0
    , gcdetails_threads                   = 0
    , gcdetails_allocated_bytes           = 0
    , gcdetails_live_bytes                = 0
    , gcdetails_large_objects_bytes       = 0
    , gcdetails_compact_bytes             = 0
    , gcdetails_slop_bytes                = 0
    , gcdetails_mem_in_use_bytes          = 0
    , gcdetails_copied_bytes              = 0
    , gcdetails_par_max_copied_bytes      = 0
#if MIN_VERSION_base(4,11,0)
    , gcdetails_par_balanced_copied_bytes = 0
#endif
    , gcdetails_sync_elapsed_ns           = 0
    , gcdetails_cpu_ns                    = 0
    , gcdetails_elapsed_ns                = 0
#if MIN_VERSION_base(4,14,1)
    , gcdetails_nonmoving_gc_sync_cpu_ns  = 0
    , gcdetails_nonmoving_gc_sync_elapsed_ns = 0
#endif
    }

-- | Specification of the metrics registered by `registerGcMetrics`.
data GcMetrics :: Symbol -> MetricType -> Type -> Type where
  -- [Counters]

  -- | Total number of bytes allocated
  BytesAllocated :: GcMetrics "rts.gc.bytes_allocated" 'CounterType ()
  -- | Number of garbage collections performed
  NumGcs :: GcMetrics "rts.gc.num_gcs" 'CounterType ()
  -- | Number of byte usage samples taken
  NumBytesUsageSamples :: GcMetrics "rts.gc.num_bytes_usage_samples" 'CounterType ()
  -- | Sum of all byte usage samples, can be used with
  -- @num_bytes_usage_samples@ to calculate averages with arbitrary weighting
  -- (if you are sampling this record multiple times).
  CumulativeBytesUsed :: GcMetrics "rts.gc.cumulative_bytes_used" 'CounterType ()
  -- | Number of bytes copied during GC
  BytesCopied :: GcMetrics "rts.gc.bytes_copied" 'CounterType ()
#if MIN_VERSION_base(4,12,0)
  -- | CPU time used by the init phase, in milliseconds. GHC 8.6+ only.
  InitCpuMs :: GcMetrics "rts.gc.init_cpu_ms" 'CounterType ()
  -- | Wall clock time spent running the init phase, in milliseconds. GHC 8.6+
  -- only.
  InitWallMs :: GcMetrics "rts.gc.init_wall_ms" 'CounterType ()
#endif
  -- | CPU time spent running mutator threads, in milliseconds. This does not
  -- include any profiling overhead or initialization.
  MutatorCpuMs :: GcMetrics "rts.gc.mutator_cpu_ms" 'CounterType ()
  -- | Wall clock time spent running mutator threads, in milliseconds. This
  -- does not include initialization.
  MutatorWallMs :: GcMetrics "rts.gc.mutator_wall_ms" 'CounterType ()
  -- | CPU time spent running GC, in milliseconds.
  GcCpuMs :: GcMetrics "rts.gc.gc_cpu_ms" 'CounterType ()
  -- | Wall clock time spent running GC, in milliseconds.
  GcWallMs :: GcMetrics "rts.gc.gc_wall_ms" 'CounterType ()
  -- | Total CPU time elapsed since program start, in milliseconds.
  CpuMs :: GcMetrics "rts.gc.cpu_ms" 'CounterType ()
  -- | Total wall clock time elapsed since start, in milliseconds.
  WallMs :: GcMetrics "rts.gc.wall_ms" 'CounterType ()

  -- [Gauges]

  -- | Maximum number of live bytes seen so far
  MaxBytesUsed :: GcMetrics "rts.gc.max_bytes_used" 'GaugeType ()
  -- | Current number of live bytes
  CurrentBytesUsed :: GcMetrics "rts.gc.current_bytes_used" 'GaugeType ()
  -- | Current number of bytes lost to slop
  CurrentBytesSlop :: GcMetrics "rts.gc.current_bytes_slop" 'GaugeType ()
  -- | Maximum number of bytes lost to slop at any one time so far
  MaxBytesSlop :: GcMetrics "rts.gc.max_bytes_slop" 'GaugeType ()
  -- | Maximum number of megabytes allocated
  PeakMegabytesAllocated :: GcMetrics "rts.gc.peak_megabytes_allocated" 'GaugeType ()
  -- | Number of bytes copied during GC, minus space held by mutable lists held by the capabilities.  Can be used with 'parMaxBytesCopied' to determine how well parallel GC utilized all cores.
  ParTotBytesCopied :: GcMetrics "rts.gc.par_tot_bytes_copied" 'GaugeType ()
  -- | Sum of number of bytes copied each GC by the most active GC thread each
  -- GC. The ratio of @par_tot_bytes_copied@ divided by @par_max_bytes_copied@
  -- approaches 1 for a maximally sequential run and approaches the number of
  -- threads (set by the RTS flag @-N@) for a maximally parallel run.
  ParMaxBytesCopied :: GcMetrics "rts.gc.par_max_bytes_copied" 'GaugeType ()
