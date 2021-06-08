{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- This module defines the metrics store and all of its operations using
-- the state type defined in "System.Metrics.Internal.State". The
-- interface presented in this module is then restricted in
-- "System.Metrics.Static" to produce the final interface.
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
-- * We bind the `Handle`s of "System.Metrics.Internal.State" to
--   specific `IORef`s in `deregisterHandles`, preventing the confusion of
--   handles from different `Store`s.

module System.Metrics.Internal.Store
    (
      -- * The metric store
      -- $metric-store
      Store
    , newStore

      -- * Identifying metrics
    , Identifier (..)

      -- * Registering metrics
      -- $registering
    , Registration
    , register
    , registerCounter
    , registerGauge
    , registerLabel
    , registerDistribution
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createLabel
    , createDistribution

      -- * Deregistering metrics
    , Deregistration
    , deregister
    , deregisterMetric
    , deregisterByName

      -- * Sampling metrics
      -- $sampling
    , Sample
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl')
import qualified Data.Text as T
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import System.Metrics.Distribution (Distribution)
import qualified System.Metrics.Distribution as Distribution
import System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import System.Metrics.Internal.State
  hiding (deregister, deregisterByName, register, registerGroup, sampleAll)
import qualified System.Metrics.Internal.State as Internal
import System.Metrics.Label (Label)
import qualified System.Metrics.Label as Label

------------------------------------------------------------------------
-- * The metric store

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

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
                -> IO Int64   -- ^ Action to read the current metric value
                -> Registration -- ^ Registration action
registerCounter identifier sample =
    registerGeneric identifier (CounterS sample)

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe. Also see 'createGauge'.
registerGauge :: Identifier -- ^ Gauge identifier
              -> IO Int64   -- ^ Action to read the current metric value
              -> Registration -- ^ Registration action
registerGauge identifier sample =
    registerGeneric identifier (GaugeS sample)

-- | Register a text metric. The provided action to read the value
-- must be thread-safe. Also see 'createLabel'.
registerLabel :: Identifier -- ^ Label identifier
              -> IO T.Text  -- ^ Action to read the current metric value
              -> Registration -- ^ Registration action
registerLabel identifier sample =
    registerGeneric identifier (LabelS sample)

-- | Register a distribution metric. The provided action to read the
-- value must be thread-safe. Also see 'createDistribution'.
registerDistribution
    :: Identifier             -- ^ Distribution identifier
    -> IO Distribution.Stats  -- ^ Action to read the current metric
    -> Registration -- ^ Registration action
registerDistribution identifier sample =
    registerGeneric identifier (DistributionS sample)

registerGeneric
  :: Identifier -- ^ Metric identifier
  -> MetricSampler -- ^ Sampling action
  -> Registration -- ^ Registration action
registerGeneric identifier sample = Registration $ \state0 ->
    let (state1, handle) = Internal.register identifier sample state0
    in  (state1, (:) handle)

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
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Data.HashMap.Strict as M
-- > import GHC.Stats
-- > import System.Metrics
-- >
-- > main = do
-- >     store <- newStore
-- >     let metrics =
-- >             [ ("num_gcs", Counter . numGcs)
-- >             , ("max_bytes_used", Gauge . maxBytesUsed)
-- >             ]
-- >     registerGroup (M.fromList metrics) getGCStats store
registerGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric names and getter functions.
    -> IO a          -- ^ Action to sample the metric group
    -> Registration -- ^ Registration action
registerGroup getters cb = Registration $ \state0 ->
    let (state1, handles) = Internal.registerGroup getters cb state0
    in  (state1, (++) handles)

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a 'Counter') with registering that reference in the store in one
-- convenient function.
--
-- Deregistration actions are not available through these functions.

-- | Create and register a zero-initialized counter.
createCounter :: Identifier -- ^ Counter identifier
              -> Store      -- ^ Metric store
              -> IO Counter
createCounter identifier store = do
    counter <- Counter.new
    _ <- register store $
          registerCounter identifier (Counter.read counter)
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: Identifier -- ^ Gauge identifier
            -> Store      -- ^ Metric store
            -> IO Gauge
createGauge identifier store = do
    gauge <- Gauge.new
    _ <- register store $
          registerGauge identifier (Gauge.read gauge)
    return gauge

-- | Create and register an empty label.
createLabel :: Identifier -- ^ Label identifier
            -> Store      -- ^ Metric store
            -> IO Label
createLabel identifier store = do
    label <- Label.new
    _ <- register store $
          registerLabel identifier (Label.read label)
    return label

-- | Create and register an event tracker.
createDistribution :: Identifier -- ^ Distribution identifier
                   -> Store      -- ^ Metric store
                   -> IO Distribution
createDistribution identifier store = do
    event <- Distribution.new
    _ <- register store $
          registerDistribution identifier (Distribution.read event)
    return event

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
sampleAll :: Store -> IO Sample
sampleAll (Store store) = readIORef store >>= Internal.sampleAll
