{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module System.Metrics.Distribution
    ( Distribution
    , new
    , add
    , addN
    , read

      -- * Gathered statistics
    , Internal.Stats
    , Internal.mean
    , Internal.variance
    , Internal.count
    , Internal.sum
    , Internal.min
    , Internal.max
    ) where

import qualified Prelude
import Prelude hiding (max, min, read, sum)

import Control.Exception (assert)
import Control.Monad (forM_, replicateM)
import qualified Data.Array as A
import Data.Primitive.ByteArray
import Data.Primitive.MachDeps (sIZEOF_INT)
import GHC.Int
import GHC.IO
import GHC.Prim

import qualified System.Metrics.Distribution.Internal as Internal
import System.Metrics.ThreadId

------------------------------------------------------------------------

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: A.Array Distrib }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Distrib
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `A.index` (tid `mod` numStripes)

------------------------------------------------------------------------

newtype Distrib = Distrib (MutableByteArray RealWorld)

sIZEOF_CACHELINE :: Int
sIZEOF_CACHELINE = 64
{-# INLINE sIZEOF_CACHELINE #-}

posLock, posCount, posMean, posSumSqDelta, posSum, posMin, posMax :: Int
-- Putting the variable-sized `Int` field first so that its offset (0)
-- does not depend on its size. This assumes that the size of `Int` is
-- at most 8 bytes.
posLock = 0 -- Int
posCount = 1 -- Int64
posMean = 2 -- Double
posSumSqDelta = 3 -- Double
posSum = 4 -- Double
posMin = 5 -- Double
posMax = 6 -- Double

newDistrib :: IO Distrib
newDistrib = do
    arr <- newAlignedPinnedByteArray sIZEOF_CACHELINE sIZEOF_CACHELINE

    writeByteArray @Int    arr posLock        0
    writeByteArray @Int64  arr posCount       0
    writeByteArray @Double arr posMean        0.0
    writeByteArray @Double arr posSumSqDelta  0.0
    writeByteArray @Double arr posSum         0.0
    writeByteArray @Double arr posMin         inf
    writeByteArray @Double arr posMax         (-inf)

    pure $ Distrib arr
  where
    inf :: Double
    inf = 1/0

withLock :: Distrib -> IO () -> IO ()
withLock distrib action = mask_ $ do
  lock distrib
  action
  unlock distrib

lock :: Distrib -> IO ()
lock (Distrib (MutableByteArray arr#)) = IO $ \s0# ->
  case spinlock arr# s0# of
    s1# -> (# s1#, () #)

spinlock
  :: MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld
spinlock arr# s0# =
  case posLock of { I# posLock# ->
  case casIntArray# arr# posLock# 0# 1# s0# of { (# s1#, r# #) ->
  case r# of
    0# -> s1#
    _ -> spinlock arr# s1#
  }}

unlock :: Distrib -> IO ()
unlock (Distrib arr) = writeByteArray @Int arr posLock 0

-- Mean and variance are computed according to
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
distribAddN :: Distrib -> Double -> Int64 -> IO ()
distribAddN distrib val n = do
  let n' = fromIntegral n :: Double
  withLock distrib $ do
    let Distrib arr = distrib

    oldCount <- readByteArray @Int64 arr posCount
    oldMean <- readByteArray @Double arr posMean
    oldSumSqDelta <- readByteArray @Double arr posSumSqDelta
    oldSum <- readByteArray @Double arr posSum
    oldMin <- readByteArray @Double arr posMin
    oldMax <- readByteArray @Double arr posMax

    let newCount = oldCount + n
        delta = val - oldMean
        newMean = oldMean + n' * delta / fromIntegral newCount
        newSumSqDelta = oldSumSqDelta + delta * (val - newMean) * n'
        newSum = oldSum + val -- Shouldn't this be `oldSum + n'*val`?
        newMin = Prelude.min oldMin val
        newMax = Prelude.max oldMax val

    writeByteArray @Int64 arr posCount newCount
    writeByteArray @Double arr posMean newMean
    writeByteArray @Double arr posSumSqDelta newSumSqDelta
    writeByteArray @Double arr posSum newSum
    writeByteArray @Double arr posMin newMin
    writeByteArray @Double arr posMax newMax

-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
distribCombine :: Distrib -> Distrib -> IO ()
distribCombine distrib (Distrib accArr) =
  withLock distrib $ do
    let Distrib arr = distrib

    count <- readByteArray @Int64 arr posCount
    mean <- readByteArray @Double arr posMean
    sumSqDelta <- readByteArray @Double arr posSumSqDelta
    sum <- readByteArray @Double arr posSum
    min <- readByteArray @Double arr posMin
    max <- readByteArray @Double arr posMax

    accCount <- readByteArray @Int64 accArr posCount
    accMean <- readByteArray @Double accArr posMean
    accSumSqDelta <- readByteArray @Double accArr posSumSqDelta
    accSum <- readByteArray @Double accArr posSum
    accMin <- readByteArray @Double accArr posMin
    accMax <- readByteArray @Double accArr posMax

    let newCount = count + accCount
        delta = mean - accMean
        count' = fromIntegral count
        countAcc' = fromIntegral accCount
        newCount' = fromIntegral newCount
        newMean = (countAcc' * accMean + count' * mean) / newCount'
        newSumSqDelta = accSumSqDelta + sumSqDelta +
          delta * delta * (countAcc' * count') / newCount'
        newSum = sum + accSum
        newMin = Prelude.min min accMin
        newMax = Prelude.max max accMax

    writeByteArray @Int64 accArr posCount newCount
    writeByteArray @Double accArr posMean newMean
    writeByteArray @Double accArr posSumSqDelta newSumSqDelta
    writeByteArray @Double accArr posSum newSum
    writeByteArray @Double accArr posMin newMin
    writeByteArray @Double accArr posMax newMax

------------------------------------------------------------------------
-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . A.fromList numStripes) `fmap`
      replicateM numStripes newDistrib

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int64 -> IO ()
addN distribution val n = do
  distrib <- myStripe distribution
  distribAddN distrib val n

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Internal.Stats
read distribution = do
  result <- newDistrib
  forM_ (A.toList $ unD distribution) $ \distrib ->
    distribCombine distrib result

  let Distrib arr = result
  count <- readByteArray @Int64 arr posCount
  mean <- readByteArray @Double arr posMean
  sumSqDelta <- readByteArray @Double arr posSumSqDelta
  sum <- readByteArray @Double arr posSum
  min <- readByteArray @Double arr posMin
  max <- readByteArray @Double arr posMax

  pure $! Internal.Stats
    { Internal.mean  = if count == 0 then 0.0 else mean
    , Internal.variance = if count == 0 then 0.0
                  else sumSqDelta / fromIntegral count
    , Internal.count = count
    , Internal.sum   = sum
    , Internal.min   = if count == 0 then 0.0 else min
    , Internal.max   = if count == 0 then 0.0 else max
    }
