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
    , register
    , registerGroup
    , deregister
    , deregisterByName

      -- * Sampling metrics
    , Sample
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Hashable
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl')
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding (read)

import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * The metric store state

type GroupId = Int

-- | The internal state of the metrics `System.Metrics.Store`.
data State = State
     { stateMetrics :: !(M.HashMap Identifier (Either MetricSampler GroupId))
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
     , stateNextId  :: {-# UNPACK #-} !Int
        -- ^ The `GroupId` to be used for the next created group.
        --
        -- Invariants: Increases monotonically; must be greater than the
        -- `GroupID`s of all existing sample groups.
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

-- | The data used to identify a metric in the store.
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
initialState = State M.empty IM.empty 0

------------------------------------------------------------------------
-- * State verification

-- | Verify the internal consistency of the state.
verifyState :: State -> Bool
verifyState state =
     checkSampleGroups state
  && checkNextGroupId state

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
        [(id', groupId) | (id', Right groupId) <- M.toList stateMetrics]
      where
        insert_ im (name, groupId) = IM.alter (putName name) groupId im
        putName identifier =
            Just . maybe (S.singleton identifier) (S.insert identifier)

groupSamplerIdentifiers :: GroupSampler -> S.HashSet Identifier
groupSamplerIdentifiers GroupSampler{..} =
  M.keysSet groupSamplerMetrics

-- | Check the following invariant:
--
-- `stateNextId` must be greater than the `GroupID`s of all existing
-- sample groups.
checkNextGroupId :: State -> Bool
checkNextGroupId State{..} = maybe True (< stateNextId) mLargestGroupId
  where
    mLargestGroupId = fst <$> IM.lookupMax stateGroups

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
        Just (Left _) -> State
            { stateMetrics = M.delete identifier stateMetrics
            , stateGroups = stateGroups
            , stateNextId = stateNextId
            }
        Just (Right groupID) -> State
            { stateMetrics = M.delete identifier stateMetrics
            , stateGroups =
                let delete_ = overGroupSamplerMetrics $ \hm ->
                        let hm' = M.delete identifier hm
                        in  if M.null hm' then Nothing else Just hm'
                in  IM.update delete_ groupID stateGroups
            , stateNextId = stateNextId
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

-- | Register a metric at the given identifier. If the identifier is
-- already in use by an existing metric, the existing metric is first
-- removed.
register
    :: Identifier -- ^ Metric identifier
    -> MetricSampler -- ^ Action to sample the metric
    -> State
    -> State
register identifier sample =
  insert identifier sample . deregister identifier

insert :: Identifier -> MetricSampler -> State -> State
insert identifier sample state = state
    { stateMetrics =
        M.insert identifier (Left sample) $ stateMetrics state
    }

-- | Register a group of metrics sharing a common sampling action. If
-- any of the given identifiers are in use by an existing metric, the
-- existing metrics are first removed. See
-- `System.Metrics.registerGroup`.
registerGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> State
registerGroup getters cb = insertGroup getters cb . delete_
  where
    delete_ state = foldl' (flip deregister) state (M.keys getters)

insertGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> State
insertGroup getters cb state
  | M.null getters = state
  | otherwise =
      let State{..} = state
      in  State
            { stateMetrics =
                M.foldlWithKey'
                  (register_ stateNextId) stateMetrics getters
            , stateGroups =
                IM.insert
                  stateNextId (GroupSampler cb getters) stateGroups
            , stateNextId = stateNextId + 1
            }
  where
    register_ groupId metrics name _ =
        M.insert name (Right groupId) metrics

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
    let metrics = stateMetrics state
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
