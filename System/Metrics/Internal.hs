{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module System.Metrics.Internal
    (
      -- * The metric store state
      State
    , GroupSampler (..)
    , MetricSampler (..)
    , Identifier (..)
    , initialState

      -- * State verification
    , verifyState

      -- * State operations
    , delete
    , register
    , registerGroup
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

-- | The 'Store' state.
data State = State
     { stateMetrics :: !(M.HashMap Identifier (Either MetricSampler GroupId))
     , stateGroups  :: !(IM.IntMap GroupSampler)
     , stateNextId  :: {-# UNPACK #-} !Int
     }

data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
     , groupSamplerMetrics :: !(M.HashMap Identifier (a -> Value))
     }

-- TODO: Rename this to Metric and Metric to SampledMetric.
data MetricSampler = CounterS !(IO Int64)
                   | GaugeS !(IO Int64)
                   | LabelS !(IO T.Text)
                   | DistributionS !(IO Distribution.Stats)

data Identifier = Identifier
    { idName :: T.Text
    , idTags :: M.HashMap T.Text T.Text
    }
    deriving (Eq, Generic, Show)

instance Hashable Identifier

initialState :: State
initialState = State M.empty IM.empty 0

------------------------------------------------------------------------
-- * State verification

-- | Verify the internal consistency of the state.
verifyState :: State -> Bool
verifyState State{..} =
      groupsFromGroups == groupsFromMetrics
  &&  maybe True (< stateNextId) largestGroupId
  where
    groupsFromGroups = getSamplerIdentifiers <$> stateGroups
    groupsFromMetrics =
      foldl' insert_ IM.empty
        [(id', groupId) | (id', Right groupId) <- M.toList stateMetrics]
      where
        insert_ im (name, groupId) = IM.alter (putName name) groupId im
        putName identifier =
            Just . maybe (S.singleton identifier) (S.insert identifier)

    largestGroupId = fst <$> IM.lookupMax stateGroups

getSamplerIdentifiers :: GroupSampler -> S.HashSet Identifier
getSamplerIdentifiers GroupSampler{..} = M.keysSet groupSamplerMetrics

------------------------------------------------------------------------
-- * State operations

-- Delete an identifier and its associated metric. When no metric is
-- registered at the identifier, the original state is returned.
delete :: Identifier -> State -> State
delete identifier state@State{..} =
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
                let delete_ = overSamplerMetrics $ \hm ->
                        let hm' = M.delete identifier hm
                        in  if M.null hm' then Nothing else Just hm'
                in  IM.update delete_ groupID stateGroups
            , stateNextId = stateNextId
            }

overSamplerMetrics ::
  (Functor f) =>
  (forall a. M.HashMap Identifier a -> f (M.HashMap Identifier a)) ->
  GroupSampler ->
  f GroupSampler
overSamplerMetrics f GroupSampler{..} =
  flip fmap (f groupSamplerMetrics) $ \groupSamplerMetrics' ->
      GroupSampler
          { groupSampleAction = groupSampleAction
          , groupSamplerMetrics = groupSamplerMetrics'
          }

register :: Identifier -> MetricSampler -> State -> State
register identifier sample =
  insert identifier sample . delete identifier

insert :: Identifier -> MetricSampler -> State -> State
insert identifier sample state = state
    { stateMetrics =
        M.insert identifier (Left sample) $ stateMetrics state
    }

registerGroup
    :: M.HashMap Identifier
       (a -> Value)  -- ^ Metric identifiers and getter functions
    -> IO a          -- ^ Action to sample the metric group
    -> State
    -> State
registerGroup getters cb = insertGroup getters cb . delete_
  where
    delete_ state = foldl' (flip delete) state (M.keys getters)

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

deregisterByName :: T.Text -> State -> State
deregisterByName name state =
    let identifiers = -- to remove
          filter (\i -> name == idName i) $ M.keys $ stateMetrics state
    in  foldl' (flip delete) state identifiers

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
