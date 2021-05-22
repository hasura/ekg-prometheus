{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic increments using 100 concurrent writers.
module Main where

import Prelude hiding (read)

import Control.Concurrent
import Control.Monad
import Criterion
import Criterion.Main (defaultMain)
import Data.Int (Int64)
import System.Metrics.Counter

main :: IO ()
main = defaultMain
  [ bench "Increment counter with multiple writers" $
      whnfIO incrementWithMultipleWriters
  ]

incrementWithMultipleWriters :: IO Int64
incrementWithMultipleWriters = do
    counter <- new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work counter iters) locks
    mapM_ takeMVar locks
    total <- read counter
    unless (fromIntegral total == n*iters) $
      error "Incorrect count!"
    pure total
  where
    n = 100
    iters = 100000

    work :: Counter -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work counter i lock = inc counter >> work counter (i - 1) lock
