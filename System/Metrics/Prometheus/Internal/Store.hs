{-# OPTIONS_HADDOCK hide #-}
-- |
-- This module defines the metrics store and all of its operations using
-- the state type defined in "System.Metrics.Prometheus.Internal.State". The
-- interface presented in this module is then restricted in
-- "System.Metrics.Prometheus.Static" to produce the final interface.
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
--
-- = Implementation summary
--
-- * We wrap the internal `State` in an `IORef`, making it suitable as a
--   global store.
--
-- * We wrap operations on the `State` and allow them to be composed,
--   then run such compositions atomically using `atomicModifyIORef'`.
--   This allows for atomic operations on the `Store`.
--
-- * We bind the `Handle`s of "System.Metrics.Prometheus.Internal.State" to
--   specific `IORef`s in `deregisterHandles`, preventing the confusion of
--   handles from different `Store`s.

module System.Metrics.Prometheus.Internal.Store
    (
      -- * The metric store
      -- $metric-store
      Store
    , newStore

      -- * Identifying metrics
    , Identifier (..)
    , HelpText

      -- * Registering metrics
      -- $registering
    , Registration
    , register
    , registerCounter
    , registerGauge
    , registerHistogram
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createHistogram

      -- * Deregistering metrics
    , Deregistration
    , deregister
    , deregisterMetric
    , deregisterByName

      -- * Sampling metrics
      -- $sampling
    , Sample
    , MetricName
    , HelpTexts
    , sampleAll
    , Value(..)
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Prelude hiding (read)

import System.Metrics.Prometheus.Counter (Counter)
import qualified System.Metrics.Prometheus.Counter as Counter
import System.Metrics.Prometheus.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Gauge as Gauge
import System.Metrics.Prometheus.Histogram (Histogram, HistogramSample)
import qualified System.Metrics.Prometheus.Histogram as Histogram
import System.Metrics.Prometheus.Internal.State
  hiding (deregister, deregisterByName, register, registerGroup, sampleAll)
import qualified System.Metrics.Prometheus.Internal.State as Internal

------------------------------------------------------------------------
-- * The metric store

-- | A mutable metric store.
newtype Store = Store (IORef State)

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = Store <$> newIORef initialState

------------------------------------------------------------------------
-- * Registering metrics

-- | An action that registers one or more metrics to a metric store.
newtype Registration =
  Registration (State -> (State, [Handle] -> [Handle]))

instance Semigroup Registration where
  Registration f <> Registration g = Registration $ \state0 ->
    let (state1, h1) = f state0
        (state2, h2) = g state1
    in  (state2, h2 . h1)

instance Monoid Registration where
  mempty = Registration $ \state -> (state, id)

-- | Atomically apply a registration action to a metrics store. Returns
-- an action to (atomically) deregisterMetric the newly registered metrics.
register
  :: Store -- ^ Metric store
  -> Registration -- ^ Registration action
  -> IO (IO ()) -- ^ Deregistration action
register (Store stateRef) (Registration f) =
    atomicModifyIORef' stateRef $ \state0 ->
        let (state1, handles') = f state0
            deregisterAction = deregisterHandles (handles' []) stateRef
        in  (state1, deregisterAction)

-- | Deregister the metrics referred to by the given handles.
deregisterHandles
  :: [Internal.Handle]
  -> IORef Internal.State
  -> IO ()
deregisterHandles handles stateRef =
    atomicModifyIORef' stateRef $ \state ->
        (foldl' (flip Internal.deregisterByHandle) state handles, ())

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
-- Also see 'createCounter'.
registerCounter :: Identifier -- ^ Counter identifier
                -> HelpText -- ^ Metric documentation
                -> IO Double  -- ^ Action to read the current metric value
                -> Registration -- ^ Registration action
registerCounter identifier helpText sample =
    registerGeneric identifier helpText (CounterS sample)

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge :: Identifier -- ^ Gauge identifier
              -> HelpText -- ^ Metric documentation
              -> IO Double  -- ^ Action to read the current metric value
              -> Registration -- ^ Registration action
registerGauge identifier helpText sample =
    registerGeneric identifier helpText (GaugeS sample)

-- | Register a histogram metric. The provided action to read the value
-- must be thread-safe. Also see 'createHistogram.
registerHistogram :: Identifier -- ^ Histogram identifier
                  -> HelpText -- ^ Metric documentation
                  -> IO HistogramSample -- ^ Action to read the current metric value
                  -> Registration -- ^ Registration action
registerHistogram identifier helpText sample =
    registerGeneric identifier helpText (HistogramS sample)

registerGeneric
  :: Identifier -- ^ Metric identifier
  -> HelpText -- ^ Metric documentation
  -> MetricSampler -- ^ Sampling action
  -> Registration -- ^ Registration action
registerGeneric identifier helpText sample = Registration $ \state0 ->
    let (state1, handle) =
          Internal.register identifier helpText sample state0
    in  (state1, (:) handle)

registerGroup
    :: M.Map
        Identifier
        (a -> Value, HelpText) -- ^ Metric names and getter functions
    -> IO a -- ^ Action to sample the metric group
    -> Registration -- ^ Registration action
registerGroup getters cb = Registration $ \state0 ->
    let (state1, handles) = Internal.registerGroup getters cb state0
    in  (state1, (++) handles)

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a `System.Metrics.Prometheus.Counter.Counter`) with registering that reference
-- in the store in one convenient function. The deregistration handles
-- are discarded.

-- | Create and register a zero-initialized counter.
createCounter :: Identifier -- ^ Counter identifier
              -> HelpText -- ^ Metric documentation
              -> Store      -- ^ Metric store
              -> IO Counter
createCounter identifier helpText store = do
    counter <- Counter.new
    _ <- register store $
          registerCounter identifier helpText (Counter.read counter)
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: Identifier -- ^ Gauge identifier
            -> HelpText -- ^ Metric documentation
            -> Store      -- ^ Metric store
            -> IO Gauge
createGauge identifier helpText store = do
    gauge <- Gauge.new
    _ <- register store $
          registerGauge identifier helpText (Gauge.read gauge)
    return gauge

-- | Create and register an empty histogram. The buckets of the
-- histogram are fixed and defined by the given upper bounds.
createHistogram :: [Histogram.UpperBound] -- ^ Upper bounds of buckets
                -> Identifier -- ^ Histogram identifier
                -> HelpText -- ^ Metric documentation
                -> Store      -- ^ Metric store
                -> IO Histogram
createHistogram upperBounds identifier helpText store = do
    histogram <- Histogram.new upperBounds
    _ <- register store $
          registerHistogram identifier helpText (Histogram.read histogram)
    return histogram

------------------------------------------------------------------------
-- * Deregistering metrics

-- | An action that deregisters metrics from a metric store.
newtype Deregistration = Deregistration (State -> State)

instance Semigroup Deregistration where
  Deregistration f <> Deregistration g = Deregistration (g . f)

instance Monoid Deregistration where
  mempty = Deregistration id

-- | Atomically apply a deregistration action to a metrics store.
deregister
  :: Store -- ^ Metric store
  -> Deregistration -- ^ Deregistration action
  -> IO ()
deregister (Store stateRef) (Deregistration f) =
    atomicModifyIORef' stateRef $ \state -> (f state, ())

-- | Deregister a metric (of any type).
deregisterMetric
  :: Identifier -- ^ Metric identifier
  -> Deregistration
deregisterMetric identifier =
  Deregistration $ Internal.deregister identifier

-- | Deregister all metrics (of any type) with the given name, that is,
-- irrespective of their tags.
deregisterByName
  :: T.Text -- ^ Metric name
  -> Deregistration
deregisterByName name = Deregistration $ Internal.deregisterByName name

------------------------------------------------------------------------
-- * Sampling metrics

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO (Sample, HelpTexts)
sampleAll (Store store) = readIORef store >>= Internal.sampleAll
