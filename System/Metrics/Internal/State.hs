{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module defines the internal state of the metrics store and all
-- operations on it.
--
-- = Warning
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
module System.Metrics.Internal.State
    (
      -- * The metric store state
      State
    , MetricSampler (..)
    , GroupSampler (..)
    , Identifier (..)
    , initialState

      -- * State verification
    , verifyState

      -- * Core operations
      -- $core-operations
    , register
    , registerGroup
    , deregister

      -- * Derived operations
      -- $derived-operations
    , Handle
    , deregisterByHandle
    , deregisterByName

      -- * Sampling metrics
    , Sample
    , sampleAll
    , Value(..)

      -- * Testing
    , SampledState (..)
    , sampleState
    , functionallyEqual
    ) where

import Control.Monad (forM)
import Data.Hashable
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as S
import Data.List (foldl', mapAccumL)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding (read)

import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * The metric store state

type GroupId = Integer
type Version = Integer

-- | The internal state of the metrics `System.Metrics.Store`.
data State = State
     { stateMetrics ::
        !(HM.HashMap Identifier (Either MetricSampler GroupId, Version))
        -- ^ A registry of all metrics in the store.
        --
        -- Identifiers are associated with a metric unless they are part
        -- of a sample group, in which case the identifier is associated
        -- with the group while the group is associated with the metric.
        --
        -- Identifiers are also associated with a `Version` number in
        -- order to differentiate between the different metrics an
        -- identifier may be associated with over time as metrics are
        -- deregistered and reregistered.
     , stateGroups  :: !(M.Map GroupId GroupSampler)
        -- ^ Actions to sample groups of metrics, indexed by `GroupId`.
        --
        -- Invariants: A `GroupSampler` for a group @g@ should sample
        -- for a metric exactly when the metric's identifier is
        -- associated with the group @g@ in the `stateMetrics` registry;
        -- sample groups must sample for at least one metric.
     , stateNextGroupId  :: !GroupId
        -- ^ The `GroupId` to be used for the next registered group.
        --
        -- Invariants: Increases monotonically; must be greater than the
        -- `GroupID`s of all existing sample groups.
     , stateNextMetricVersion :: !Version
        -- ^ The version to be used for the next registered metric.
        --
        -- Invariants: Increases monotonically; must be greater than the
        -- metric versions of all existing metrics.
     }

-- TODO: Rename this to Metric and Metric to SampledMetric.
-- | An action to read the current value of a metric. Needs to be
-- thread-safe.
data MetricSampler
  = CounterS !(IO Int64)
    -- ^ Action to sample a counter
  | GaugeS !(IO Int64)
    -- ^ Action to sample a gauge
  | LabelS !(IO T.Text)
    -- ^ Action to sample a label
  | DistributionS !(IO Distribution.Stats)
    -- ^ Action to sample a distribution

-- | An action to sample a group of metrics together.
--
-- Can be useful for efficiency or obtaining a consistent view of
-- multiple metrics. Needs to be thread safe.
--
data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
        -- ^ Action to sample the metric group
     , groupSamplerMetrics :: !(HM.HashMap Identifier (a -> Value))
        -- ^ Metric identifiers and getter functions.
     }

-- | The data used by the store to identify a metric.
data Identifier = Identifier
    { idName :: T.Text
      -- ^ The name of the metric
    , idTags :: HM.HashMap T.Text T.Text
      -- ^ The key-value pairs associated with the metric
    }
    deriving (Eq, Generic, Show)

instance Hashable Identifier

-- | The initial state of a new store.
initialState :: State
initialState = State HM.empty M.empty 0 0

------------------------------------------------------------------------
-- * State verification

-- | Verify the internal consistency of the state.
verifyState :: State -> Bool
verifyState state =
     checkSampleGroups state
  && checkNextGroupId state
  && checkNextMetricVersion state

-- | Check the following invariants:
--
-- A `GroupSampler` for a group @g@ should sample for a metric exactly
-- when the metric's identifier is associated with the group @g@ in the
-- `stateMetrics` registry; sample groups must sample for at least one
-- metric.
checkSampleGroups :: State -> Bool
checkSampleGroups State{..} =
  -- Note: The check for non-empty sample groups is implicit.
  groupsFromGroups == groupsFromMetrics
  where
    groupsFromGroups = groupSamplerIdentifiers <$> stateGroups

    groupsFromMetrics =
      foldl' insert_ M.empty
        [(id', groupId) |
          (id', (Right groupId, _)) <- HM.toList stateMetrics]
      where
        insert_ m (name, groupId) = M.alter (putName name) groupId m
        putName identifier =
            Just . maybe (S.singleton identifier) (S.insert identifier)

groupSamplerIdentifiers :: GroupSampler -> S.HashSet Identifier
groupSamplerIdentifiers GroupSampler{..} =
  HM.keysSet groupSamplerMetrics

-- | Check the following invariant:
--
-- `stateNextGroupId` must be greater than the `GroupID`s of all existing
-- sample groups.
checkNextGroupId :: State -> Bool
checkNextGroupId State{..} =
    maybe True (< stateNextGroupId) mLargestGroupId
  where
    mLargestGroupId = fst <$> M.lookupMax stateGroups

-- | Check the following invariant:
--
-- `stateNextMetricVersion` must be greater than the metric versions of
-- all existing metrics.
checkNextMetricVersion :: State -> Bool
checkNextMetricVersion State{..} =
    let versions = map snd $ HM.elems stateMetrics
    in  all (< stateNextMetricVersion) versions

------------------------------------------------------------------------
-- * Core operations

-- $core-operations
-- These "core" operations represent the complete set of ways in which
-- the `State` may be modified. We must make sure that these ops
-- maintain the `State` invariants.

-- | Deregister the metric at the given identifier. When no metric is
-- registered at the identifier, the original state is returned.
deregister
    :: Identifier -- ^ Metric identifier
    -> State
    -> State
deregister identifier state =
    case HM.lookup identifier (stateMetrics state) of
        Nothing -> state
        Just (Left _, _) -> state
            { stateMetrics = HM.delete identifier $ stateMetrics state
            }
        Just (Right groupID, _) -> state
            { stateMetrics = HM.delete identifier $ stateMetrics state
            , stateGroups =
                let delete_ = overGroupSamplerMetrics $ \hm ->
                        let hm' = HM.delete identifier hm
                        in  if HM.null hm' then Nothing else Just hm'
                in  M.update delete_ groupID $ stateGroups state
            }

overGroupSamplerMetrics ::
  (Functor f) =>
  (forall a. HM.HashMap Identifier a -> f (HM.HashMap Identifier a)) ->
  GroupSampler ->
  f GroupSampler
overGroupSamplerMetrics f GroupSampler{..} =
  flip fmap (f groupSamplerMetrics) $ \groupSamplerMetrics' ->
      GroupSampler
          { groupSampleAction = groupSampleAction
          , groupSamplerMetrics = groupSamplerMetrics'
          }

-- | Register a metric at the given identifier. If the identifier is
-- already in use by an existing metric, the existing metric is first
-- removed. Returns a handle for deregistering the registered metric.
register
    :: Identifier -- ^ Metric identifier
    -> MetricSampler -- ^ Action to sample the metric
    -> State -- ^ State
    -> (State, Handle) -- ^ (New state, deregistration handle)
register identifier sample =
  insertMetricSampler identifier sample . deregister identifier

insertMetricSampler
  :: Identifier -> MetricSampler -> State -> (State, Handle)
insertMetricSampler identifier sampler state0 =
  let stateNextMetricVersion0 = stateNextMetricVersion state0
      state1 = state0
        { stateMetrics =
            HM.insert
              identifier
              (Left sampler, stateNextMetricVersion0)
              (stateMetrics state0)
        , stateNextMetricVersion = stateNextMetricVersion0 + 1
        }
      handle = Handle identifier stateNextMetricVersion0
  in  (state1, handle)

-- | Register a group of metrics sharing a common sampling action. If
-- any of the given identifiers are in use by an existing metric, the
-- existing metrics are first removed. Returns handles for deregistering
-- the registered metrics. See `System.Metrics.registerGroup`.
registerGroup
    :: HM.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> (State, [Handle])
registerGroup getters cb = insertGroup getters cb . delete_
  where
    delete_ state = foldl' (flip deregister) state (HM.keys getters)

insertGroup
    :: HM.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> (State, [Handle])
insertGroup getters cb state0
  | HM.null getters = (state0, [])
  | otherwise =
      let (state1, groupId) =
            insertGroupSampler (GroupSampler cb getters) state0
      in  mapAccumL (insertGroupReference groupId) state1 $
            HM.keys getters

insertGroupSampler :: GroupSampler -> State -> (State, GroupId)
insertGroupSampler groupSampler state0 =
  let stateNextGroupId0 = stateNextGroupId state0
      state1 = state0
        { stateGroups =
            M.insert stateNextGroupId0 groupSampler (stateGroups state0)
        , stateNextGroupId = stateNextGroupId0 + 1
        }
  in  (state1, stateNextGroupId0)

insertGroupReference
  :: GroupId -> State -> Identifier -> (State, Handle)
insertGroupReference groupId state0 identifier =
  let stateNextMetricVersion0 = stateNextMetricVersion state0
      state1 = state0
        { stateMetrics =
            HM.insert
              identifier
              (Right groupId, stateNextMetricVersion0)
              (stateMetrics state0)
        , stateNextMetricVersion = stateNextMetricVersion0 + 1
        }
      handle = Handle identifier stateNextMetricVersion0
  in  (state1, handle)

------------------------------------------------------------------------
-- * Derived operations

-- $derived-operations
-- These "derived" operations must only make modifications to the
-- `State` through the "core" operations. This is so that we can deduce
-- that the derived ops preserve the `State` invariants as long as the
-- core ops do.

-- | A reference to a particular version of the metric at an identifier.
--
-- A value of this type should never be exposed in order to prevent it
-- from being applied to the wrong `State`.
data Handle = Handle Identifier Version

-- | Deregister the particular metric referenced by the handle. That is,
-- deregister the metric at the given identifier, but only if its
-- version matches that held by the `Handle`.
deregisterByHandle :: Handle -> State -> State
deregisterByHandle (Handle identifier version) state =
    case HM.lookup identifier (stateMetrics state) of
        Nothing -> state
        Just (_, version') ->
            if version == version' then
                deregister identifier state
            else
                state

-- | Deregister all metrics (of any type) with the given name, ignoring
-- tags.
deregisterByName
    :: T.Text -- ^ Metric name
    -> State
    -> State
deregisterByName name state =
    let identifiers = -- Identifiers to be removed
          filter (\i -> name == idName i) $ HM.keys $ stateMetrics state
    in  foldl' (flip deregister) state identifiers

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | A sample of some metrics.
type Sample = HM.HashMap Identifier Value

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: State -> IO Sample
sampleAll state = do
    let metrics = HM.map fst $ stateMetrics state
        groups = stateGroups state
    cbSample <- sampleGroups $ M.elems groups
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample
    return $! HM.fromList allSamples

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO [(Identifier, Value)]
sampleGroups cbSamplers = concat `fmap` mapM runOne cbSamplers
  where
    runOne :: GroupSampler -> IO [(Identifier, Value)]
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! map (\ (identifier, f) -> (identifier, f a))
                      (HM.toList groupSamplerMetrics)

-- | The value of a sampled metric.
data Value = Counter {-# UNPACK #-} !Int64
           | Gauge {-# UNPACK #-} !Int64
           | Label {-# UNPACK #-} !T.Text
           | Distribution !Distribution.Stats
           deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Value
sampleOne (CounterS m)      = Counter <$> m
sampleOne (GaugeS m)        = Gauge <$> m
sampleOne (LabelS m)        = Label <$> m
sampleOne (DistributionS m) = Distribution <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: HM.HashMap Identifier (Either MetricSampler GroupId)
            -> IO [(Identifier, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- HM.toList m]) $
        \(name, ref) -> do
            val <- sampleOne ref
            return (name, val)

------------------------------------------------------------------------
-- * Testing

-- | A version of `State` where the samplers have been replaced by their
-- results. Intended to be used in tests where the samplers are all of
-- the form @pure x@ for some @x@.
data SampledState = SampledState
     { sampledStateMetrics ::
        !(HM.HashMap Identifier (Either Value GroupId, Version))
     , sampledStateGroups ::
        !(M.Map GroupId (HM.HashMap Identifier Value))
     , sampledStateNextGroupId :: !GroupId
     , sampledStateNextMetricVersion :: !Version
     }
  deriving (Show, Eq)

-- | Run all the sampling actions in the state and replace them with
-- their results.
sampleState :: State -> IO SampledState
sampleState State{..} = do
  sampledMetrics <- traverse sampleMetric stateMetrics
  sampledGroups <- traverse sampleGroupSampler stateGroups
  pure $ SampledState
     { sampledStateMetrics = sampledMetrics
     , sampledStateGroups = sampledGroups
     , sampledStateNextGroupId = stateNextGroupId
     , sampledStateNextMetricVersion = stateNextMetricVersion
     }
  where
    sampleMetric
      :: (Either MetricSampler GroupId, Version)
      -> IO (Either Value GroupId, Version)
    sampleMetric (Right groupId, version) =
      pure (Right groupId, version)
    sampleMetric (Left sample, version) = do
      value <- sampleOne sample
      pure (Left value, version)

    sampleGroupSampler
      :: GroupSampler -> IO (HM.HashMap Identifier Value)
    sampleGroupSampler GroupSampler{..} =
      (\r -> HM.map ($ r) groupSamplerMetrics) <$> groupSampleAction

-- | Test for equality ignoring `MetricId`s and `GroupId`s.
--
-- This test assumes that each `GroupSampler` in `stateGroups` is
-- unique, which follows from the `State` invariants.
functionallyEqual :: SampledState -> SampledState -> Bool
functionallyEqual state1 state2 = withoutIds state1 == withoutIds state2
  where
    withoutIds
      :: SampledState
      -> HM.HashMap Identifier
                    (Either Value (Maybe (HM.HashMap Identifier Value)))
    withoutIds state =
      flip HM.map (sampledStateMetrics state) $
        \(e, _) -> case e of
          Left value -> Left value
          Right groupId ->
            Right $ M.lookup groupId (sampledStateGroups state)
