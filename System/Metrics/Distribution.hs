{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

module System.Metrics.Distribution
    (
      -- * Warning
      -- $warning

      -- * Distributions
      Distribution
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

import Prelude hiding (max, min, read, sum)

import Control.Monad (forM_, replicateM)
import qualified Data.Array as A
import Data.Primitive.ByteArray
import GHC.Float
import GHC.Int
import GHC.IO
import GHC.Prim
import Test.Inspection

import qualified System.Metrics.Distribution.Internal as Internal
import System.Metrics.ThreadId

------------------------------------------------------------------------
-- * Warning

-- $warning
-- Warning: On 32-bit platforms, this implementation may only receive up
-- to 2^31 values. Adding a number of values beyond this limit may
-- result in nonsensical results.

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
{-# INLINE posLock #-}
{-# INLINE posCount #-}
{-# INLINE posMean #-}
{-# INLINE posSumSqDelta #-}
{-# INLINE posSum #-}
{-# INLINE posMin #-}
{-# INLINE posMax #-}

newDistrib :: IO Distrib
newDistrib = do
    -- We pad to 64 bytes (an x86 cache line) to try to avoid false sharing.
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

spinlock#
  :: MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld
spinlock# arr s0 =
  case posLock of { I# posLock' ->
  case casIntArray# arr posLock' 0# 1# s0 of { (# s1, r #) ->
  case r of
    0# -> s1
    _ -> spinlock# arr s1
  }}

unlock# :: MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld
unlock# arr s0 =
  case posLock of { I# posLock' ->
  case writeIntArray# arr posLock' 0# s0 of { s1 ->
  s1
  }}

-- | Mean and variance are computed according to
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
distribAddN :: Distrib -> Double -> Int -> IO ()
distribAddN (Distrib (MutableByteArray arr)) valBox nBox = IO $ \s0 ->
  case nBox of { I# n ->
  case valBox of { D# val ->
  case distribAddN# arr val n s0 of { s1 ->
  (# s1, () #)
  }}}

-- | Mean and variance are computed according to
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
distribAddN#
  :: MutableByteArray# RealWorld
  -> Double#
  -> Int#
  -> State# RealWorld
  -> State# RealWorld
distribAddN# arr val n s0 =
  -- Get data positions
  case posCount of { I# posCount' ->
  case posMean of { I# posMean' ->
  case posSumSqDelta of { I# posSumSqDelta' ->
  case posSum of { I# posSum' ->
  case posMin of { I# posMin' ->
  case posMax of { I# posMax' ->

  -- Convert `n` to Double
  case int2Double# n of { n' ->

  -- Lock
  case spinlock# arr s0 of { s1 ->

  -- Read values
  case readInt64Array# arr posCount' s1 of { (# s2, oldCount #) ->
  case readDoubleArray# arr posMean' s2 of { (# s3, oldMean #) ->
  case readDoubleArray# arr posSumSqDelta' s3 of { (# s4, oldSumSqDelta #) ->
  case readDoubleArray# arr posSum' s4 of { (# s5, oldSum #) ->
  case readDoubleArray# arr posMin' s5 of { (# s6, oldMin #) ->
  case readDoubleArray# arr posMax' s6 of { (# s7, oldMax #) ->

  -- Compute new values
  case oldCount +# n of { newCount ->
  case val -## oldMean of { delta ->
  case int2Double# newCount of { newCount' ->
  case oldMean +##
    n' *## delta /## newCount' of { newMean ->
  case oldSumSqDelta +##
    delta *## delta *##
    (n' *## int2Double# oldCount) /## newCount' of { newSumSqDelta ->
  case oldSum +## n' *## val of { newSum ->
  case (case val <## oldMin of { 0# -> oldMin; _ -> val }) of { newMin ->
  case (case val >## oldMax of { 0# -> oldMax; _ -> val }) of { newMax ->

  -- Write new values
  case writeInt64Array# arr posCount' newCount s7 of { s8 ->
  case writeDoubleArray# arr posMean' newMean s8 of { s9 ->
  case writeDoubleArray# arr posSumSqDelta' newSumSqDelta s9 of { s10 ->
  case writeDoubleArray# arr posSum' newSum s10 of { s11 ->
  case writeDoubleArray# arr posMin' newMin s11 of { s12 ->
  case writeDoubleArray# arr posMax' newMax s12 of { s13 ->

  -- Unlock
  case unlock# arr s13 of { s14 ->
  s14
  }}}}}}}}}}}}}}}}}}}}}}}}}}}}}

-- | Adds the data of the left distribution to that of the right
-- distribution using
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
distribCombine :: Distrib -> Distrib -> IO ()
distribCombine  (Distrib (MutableByteArray arr))
                (Distrib (MutableByteArray accArr)) = IO $ \s0 ->
  case distribCombine# arr accArr s0 of { s1 ->
  (# s1, () #)
  }

distribCombine#
  :: MutableByteArray# RealWorld
  -> MutableByteArray# RealWorld
  -> State# RealWorld
  -> State# RealWorld
distribCombine# arr accArr s0 =
  -- Get data positions
  case posCount of { I# posCount' ->
  case posMean of { I# posMean' ->
  case posSumSqDelta of { I# posSumSqDelta' ->
  case posSum of { I# posSum' ->
  case posMin of { I# posMin' ->
  case posMax of { I# posMax' ->

  -- Lock
  case spinlock# arr s0 of { s1 ->

  -- Read count from left array
  case readInt64Array# arr posCount' s1 of { (# s2, count #) ->

  -- If the left array has no data, do not combine its data with that of
  -- the right array. This is to avoid `NaN`s from divisons by zero when
  -- the right array also has no data.
  case count ># 0# of
    0# ->
      -- Unlock
      case unlock# arr s2 of { s3 ->
      s3
      }
    _ ->
      -- Read other values from left array
      case readDoubleArray# arr posMean' s2 of { (# s3, mean #) ->
      case readDoubleArray# arr posSumSqDelta' s3 of { (# s4, sumSqDelta #) ->
      case readDoubleArray# arr posSum' s4 of { (# s5, sum #) ->
      case readDoubleArray# arr posMin' s5 of { (# s6, min #) ->
      case readDoubleArray# arr posMax' s6 of { (# s7, max #) ->

      -- Read values from right array
      case readInt64Array# accArr posCount' s7 of { (# s8, accCount #) ->
      case readDoubleArray# accArr posMean' s8 of { (# s9, accMean #) ->
      case readDoubleArray# accArr posSumSqDelta' s9 of { (# s10, accSumSqDelta #) ->
      case readDoubleArray# accArr posSum' s10 of { (# s11, accSum #) ->
      case readDoubleArray# accArr posMin' s11 of { (# s12, accMin #) ->
      case readDoubleArray# accArr posMax' s12 of { (# s13, accMax #) ->

      -- Compute new values
      case count +# accCount of { newCount ->
      case mean -## accMean of { delta ->
      case int2Double# count of { count' ->
      case int2Double# accCount of { accCount' ->
      case int2Double# newCount of { newCount' ->
      case (accCount' *## accMean +## count' *## mean) /## newCount' of { newMean ->
      case accSumSqDelta +## sumSqDelta +##
        delta *## delta *##
        (accCount' *## count') /## newCount' of { newSumSqDelta ->
      case accSum +## sum of { newSum ->
      case (case min <## accMin of { 0# -> accMin; _ -> min }) of { newMin ->
      case (case max >## accMax of { 0# -> accMax; _ -> max }) of { newMax ->

      -- Write new values
      case writeInt64Array# accArr posCount' newCount s13 of { s14 ->
      case writeDoubleArray# accArr posMean' newMean s14 of { s15 ->
      case writeDoubleArray# accArr posSumSqDelta' newSumSqDelta s15 of { s16 ->
      case writeDoubleArray# accArr posSum' newSum s16 of { s17 ->
      case writeDoubleArray# accArr posMin' newMin s17 of { s18 ->
      case writeDoubleArray# accArr posMax' newMax s18 of { s19 ->

      -- Unlock
      case unlock# arr s19 of { s20 ->
      s20
      }}}}}}}}}}}}}}}}}}}}}}}}}}}}
  }}}}}}}}

-- Ensure that functions that hold locks never allocate memory. If they
-- did, threads running those functions could receive exceptions or be
-- descheduled by the runtime while holding the lock, which could result
-- in deadlock or severe performance degredation, respectively.
inspect $ mkObligation 'distribAddN# NoAllocation
inspect $ mkObligation 'distribCombine# NoAllocation

------------------------------------------------------------------------
-- * Distributions

-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . A.fromList numStripes) `fmap`
      replicateM numStripes newDistrib

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int -> IO ()
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
