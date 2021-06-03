{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- This module defines the internal state of the metrics store.
--
-- = Warning
-- This module is considered __internal__.
--
-- The contents of this module may change in any way whatsoever
-- and without any warning between minor versions of this package.
module System.Metrics.Internal
    (
      -- * The metric store state
      State
    , MetricSampler (..)
    , GroupSampler (..)
    , Identifier (..)
    , initialState

      -- * State verification
    , verifyState

      -- * State operations
      -- TODO: Think about order of presentation
    , register
    , registerGroup
    , deregister
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
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Hashable
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl', mapAccumL)
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding (read)

import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * The metric store state

type GroupId = Int
type Version = Integer

-- | The internal state of the metrics `System.Metrics.Store`.
data State = State
     { stateMetrics ::
        !(M.HashMap Identifier (Either MetricSampler GroupId, Version))
        -- ^ A registry of all metrics in the store.
        --
        -- Identifiers are associated with a metric unless they are part
        -- of a sample group, in which case the identifier is associated
        -- with the group while the group is associated with the metric.
     , stateGroups  :: !(IM.IntMap GroupSampler)
        -- ^ Actions to sample groups of metrics, indexed by `GroupId`.
        --
        -- Invariants: A `GroupSampler` for a group @g@ should sample
        -- for a metric exactly when the metric's identifier is
        -- associated with the group @g@ in the `stateMetrics` registry;
        -- sample groups must sample for at least one metric.
     , stateNextGroupId  :: {-# UNPACK #-} !Int
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
     , groupSamplerMetrics :: !(M.HashMap Identifier (a -> Value))
        -- ^ Metric identifiers and getter functions.
     }

-- | The data used by the store to identify a metric.
data Identifier = Identifier
    { idName :: T.Text
      -- ^ The name of the metric
    , idTags :: M.HashMap T.Text T.Text
      -- ^ The key-value pairs associated with the metric
    }
    deriving (Eq, Generic, Show)

instance Hashable Identifier

-- | The initial state of a new store.
initialState :: State
initialState = State M.empty IM.empty 0 0

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
      foldl' insert_ IM.empty
        [(id', groupId) |
          (id', (Right groupId, _)) <- M.toList stateMetrics]
      where
        insert_ im (name, groupId) = IM.alter (putName name) groupId im
        putName identifier =
            Just . maybe (S.singleton identifier) (S.insert identifier)

groupSamplerIdentifiers :: GroupSampler -> S.HashSet Identifier
groupSamplerIdentifiers GroupSampler{..} =
  M.keysSet groupSamplerMetrics

-- | Check the following invariant:
--
-- `stateNextGroupId` must be greater than the `GroupID`s of all existing
-- sample groups.
checkNextGroupId :: State -> Bool
checkNextGroupId State{..} =
    maybe True (< stateNextGroupId) mLargestGroupId
  where
    mLargestGroupId = fst <$> IM.lookupMax stateGroups

-- | Check the following invariant:
--
-- `stateNextMetricVersion` must be greater than the metric versions of
-- all existing metrics.
checkNextMetricVersion :: State -> Bool
checkNextMetricVersion State{..}
  | null metricVersions = True
  | otherwise = maximum metricVersions < stateNextMetricVersion
  where
    metricVersions = map snd $ M.elems stateMetrics

------------------------------------------------------------------------
-- * State operations

-- | Deregister the metric at the given identifier. When no metric is
-- registered at the identifier, the original state is returned.
deregister
    :: Identifier -- ^ Metric identifier
    -> State
    -> State
deregister identifier state@State{..} =
    case M.lookup identifier stateMetrics of
        Nothing -> state
        Just (Left _, _) -> State
            { stateMetrics = M.delete identifier stateMetrics
            , stateGroups = stateGroups
            , stateNextGroupId = stateNextGroupId
            , stateNextMetricVersion = stateNextMetricVersion
            }
        Just (Right groupID, _) -> State
            { stateMetrics = M.delete identifier stateMetrics
            , stateGroups =
                let delete_ = overGroupSamplerMetrics $ \hm ->
                        let hm' = M.delete identifier hm
                        in  if M.null hm' then Nothing else Just hm'
                in  IM.update delete_ groupID stateGroups
            , stateNextGroupId = stateNextGroupId
            , stateNextMetricVersion = stateNextMetricVersion
            }

overGroupSamplerMetrics ::
  (Functor f) =>
  (forall a. M.HashMap Identifier a -> f (M.HashMap Identifier a)) ->
  GroupSampler ->
  f GroupSampler
overGroupSamplerMetrics f GroupSampler{..} =
  flip fmap (f groupSamplerMetrics) $ \groupSamplerMetrics' ->
      GroupSampler
          { groupSampleAction = groupSampleAction
          , groupSamplerMetrics = groupSamplerMetrics'
          }

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
    case M.lookup identifier (stateMetrics state) of
        Nothing -> state
        Just (_, version') ->
            if version == version' then
                deregister identifier state
            else
                state

-- | Deregisters each of the given handles. See `deregisterByHandle`.
deregisterByHandles :: [Handle] -> State -> State
deregisterByHandles handles state =
  foldl' (flip deregisterByHandle) state handles

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
insertMetricSampler identifier sampler State{..} =
  let state = State
        { stateMetrics =
            M.insert
              identifier
              (Left sampler, stateNextMetricVersion)
              stateMetrics
        , stateGroups = stateGroups
        , stateNextGroupId = stateNextGroupId
        , stateNextMetricVersion = stateNextMetricVersion + 1
        }
      handle = Handle identifier stateNextMetricVersion
  in  (state, handle)

-- | Register a group of metrics sharing a common sampling action. If
-- any of the given identifiers are in use by an existing metric, the
-- existing metrics are first removed. Returns handles for deregistering
-- the registered metrics. See `System.Metrics.registerGroup`.
registerGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> (State, [Handle])
registerGroup getters cb = insertGroup getters cb . delete_
  where
    delete_ state = foldl' (flip deregister) state (M.keys getters)

insertGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> (State, [Handle])
insertGroup getters cb state
  | M.null getters = (state, [])
  | otherwise =
      let (state', groupId) = insertGroup state
      in  mapAccumL (insertGroupReference groupId) state' $ M.keys getters
  where
    insertGroup :: State -> (State, GroupId)
    insertGroup State{..} =
      let state' = State
            { stateMetrics = stateMetrics
            , stateGroups =
                IM.insert
                  stateNextGroupId (GroupSampler cb getters) stateGroups
            , stateNextGroupId = stateNextGroupId + 1
            , stateNextMetricVersion = stateNextMetricVersion
            }
      in  (state', stateNextGroupId)

    insertGroupReference
      :: GroupId -> State -> Identifier -> (State, Handle)
    insertGroupReference groupId State{..} identifier =
      let state = State
            { stateMetrics =
                M.insert
                  identifier
                  (Right groupId, stateNextMetricVersion)
                  stateMetrics
            , stateGroups = stateGroups
            , stateNextGroupId = stateNextGroupId
            , stateNextMetricVersion = stateNextMetricVersion + 1
            }
          handle = Handle identifier stateNextMetricVersion
      in  (state, handle)

-- | Deregister all metrics (of any type) with the given name, ignoring
-- tags.
deregisterByName
    :: T.Text -- ^ Metric name
    -> State
    -> State
deregisterByName name state =
    let identifiers = -- to be removed
          filter (\i -> name == idName i) $ M.keys $ stateMetrics state
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
type Sample = M.HashMap Identifier Value

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: State -> IO Sample
sampleAll state = do
    let metrics = M.map fst $ stateMetrics state
        groups = stateGroups state
    cbSample <- sampleGroups $ IM.elems groups
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample
    return $! M.fromList allSamples

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO [(Identifier, Value)]
sampleGroups cbSamplers = concat `fmap` sequence (map runOne cbSamplers)
  where
    runOne :: GroupSampler -> IO [(Identifier, Value)]
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! map (\ (identifier, f) -> (identifier, f a))
                      (M.toList groupSamplerMetrics)

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
readAllRefs :: M.HashMap Identifier (Either MetricSampler GroupId)
            -> IO [(Identifier, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- M.toList m]) $ \ (name, ref) -> do
        val <- sampleOne ref
        return (name, val)

------------------------------------------------------------------------
-- * Testing

-- | A version of `State` where the samplers have been replaced by their
-- results. Intended to be used in tests where the samplers are all of
-- the form @pure x@ for some @x@.
data SampledState = SampledState
     { sampledStateMetrics ::
        !(M.HashMap Identifier (Either Value GroupId, Version))
     , sampledStateGroups :: !(IM.IntMap (M.HashMap Identifier Value))
     , sampledStateNextGroupId :: {-# UNPACK #-} !Int
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

    sampleGroupSampler :: GroupSampler -> IO (M.HashMap Identifier Value)
    sampleGroupSampler GroupSampler{..} =
      (\r -> M.map ($ r) groupSamplerMetrics) <$> groupSampleAction
