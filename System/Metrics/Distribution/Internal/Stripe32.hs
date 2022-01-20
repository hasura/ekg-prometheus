module System.Metrics.Distribution.Internal.Stripe32
    ( Stripe
    , newStripe
    , stripeAddN
    , stripeCombine
    , readStripe
    ) where

import Control.Monad ((<$!>), when)
import Data.Atomics (atomicModifyIORefCAS_)
import Data.IORef
import Data.Int (Int64)

import qualified System.Metrics.Distribution.Internal as Internal

-- We want at least 64 bits in order to avoid overflow of the count of
-- samples added to the distribution.

-- If the machine word size less than 64 bits, the primitive integer
-- operations may be truncated to 32 bits. In this case, we fall back to
-- `IORef`s and `atomicModifyIORefCAS` to prevent overflow. This is much
-- slower than the approach taken in the
-- "System.Metrics.Distribution.Internal.Stripe64" module.

newtype Stripe = Stripe (IORef Distrib)

data Distrib = Distrib
  { dMean :: !Double
  , dSumSqDelta :: !Double
  , dCount :: !Int64
  , dSum   :: !Double
  , dMin   :: !Double
  , dMax   :: !Double
  }

newStripe :: IO Stripe
newStripe = Stripe <$!> newIORef Distrib
  { dMean  = 0
  , dSumSqDelta = 0
  , dCount = 0
  , dSum   = 0
  , dMin   = inf
  , dMax   = -inf
  }
  where
    inf :: Double
    inf = 1/0

-- | Mean and variance are computed according to
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
stripeAddN :: Stripe -> Double -> Int -> IO ()
stripeAddN (Stripe ref) val n = atomicModifyIORefCAS_ ref $ \dist ->
  let n' = fromIntegral n
      newCount = fromIntegral n + dCount dist
      newCount' = fromIntegral newCount
      delta = val - dMean dist
  in  Distrib
        { dMean = dMean dist + n' * delta / newCount'
        , dSumSqDelta = dSumSqDelta dist +
            delta * delta * (n' * fromIntegral (dCount dist)) / newCount'
        , dCount = newCount
        , dSum   = dSum dist + n' * val
        , dMin   = min val (dMin dist)
        , dMax   = max val (dMax dist)
        }

-- | Adds the data of the left distribution to that of the right
-- distribution using
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm
stripeCombine :: Stripe -> Stripe -> IO ()
stripeCombine (Stripe ref) (Stripe accRef) = do
  dist <- readIORef ref
  -- If the left stripe has no data, do not combine its data with that of
  -- the right stripe. This is to avoid `NaN`s from divisons by zero when
  -- the right stripe also has no data.
  when (dCount dist > 0) $
    modifyIORef' accRef $ \accDist ->
      let count = dCount dist
          mean = dMean dist
          accCount = dCount accDist
          accMean = dMean accDist
          newCount = count + accCount
          delta = mean - accMean
          count' = fromIntegral count
          accCount' = fromIntegral accCount
          newCount' = fromIntegral newCount
      in  Distrib
            { dMean = (accCount' * accMean + count' * mean) / newCount'
            , dSumSqDelta = dSumSqDelta accDist + dSumSqDelta dist +
                delta * delta * (accCount' * count') / newCount'
            , dCount = newCount
            , dSum   = dSum accDist + dSum dist
            , dMin   = min (dMin accDist) (dMin dist)
            , dMax   = max (dMax accDist) (dMax dist)
            }

readStripe :: Stripe -> IO Internal.Stats
readStripe (Stripe ref) = do
  dist <- readIORef ref
  let count = dCount dist
  pure $! Internal.Stats
    { Internal.mean  = if count == 0 then 0.0 else dMean dist
    , Internal.variance = if count == 0 then 0.0
                            else dSumSqDelta dist / fromIntegral count
    , Internal.count = dCount dist
    , Internal.sum   = dSum dist
    , Internal.min   = if count == 0 then 0.0 else dMin dist
    , Internal.max   = if count == 0 then 0.0 else dMax dist
    }
