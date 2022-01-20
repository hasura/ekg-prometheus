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

#include "MachDeps.h"

import Prelude hiding (max, min, read, sum)

import Control.Monad (forM_, replicateM)
import qualified Data.Array as A
import qualified System.Metrics.Distribution.Internal as Internal
import System.Metrics.ThreadId

#if WORD_SIZE_IN_BITS >= 64
import System.Metrics.Distribution.Internal.Stripe64
  (Stripe, newStripe, readStripe, stripeAddN, stripeCombine)
#else
import System.Metrics.Distribution.Internal.Stripe32
  (Stripe, newStripe, readStripe, stripeAddN, stripeCombine)
#endif

------------------------------------------------------------------------

-- | An metric for tracking events.
newtype Distribution = Distribution { unD :: A.Array Stripe }

-- | Number of lock stripes. Should be greater or equal to the number
-- of HECs.
numStripes :: Int
numStripes = 8

-- | Get the stripe to use for this thread.
myStripe :: Distribution -> IO Stripe
myStripe distrib = do
    tid <- myCapability
    return $! unD distrib `A.index` (tid `mod` numStripes)

------------------------------------------------------------------------
-- * Distributions

-- Exposed API

-- | Create a new distribution.
new :: IO Distribution
new = (Distribution . A.fromList numStripes) `fmap`
      replicateM numStripes newStripe

-- | Add a value to the distribution.
add :: Distribution -> Double -> IO ()
add distrib val = addN distrib val 1

-- | Add the same value to the distribution N times.
addN :: Distribution -> Double -> Int -> IO ()
addN distribution val n = do
  stripe <- myStripe distribution
  stripeAddN stripe val n

-- | Get the current statistical summary for the event being tracked.
read :: Distribution -> IO Internal.Stats
read distribution = do
  result <- newStripe
  forM_ (A.toList $ unD distribution) $ \stripe ->
    stripeCombine stripe result
  readStripe result
