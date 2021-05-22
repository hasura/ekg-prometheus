{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | Perform 100,000 atomic sample additions using 100 concurrent
-- writers.
module Main where

import Prelude hiding (read)

import Control.Concurrent
import Control.Monad
import Criterion
import Criterion.Main (defaultMain)
import Data.Int (Int64)
import System.Metrics.Distribution

main :: IO ()
main = defaultMain
  [ bench "Add to distribution with multiple writers" $
      whnfIO addWithMultipleWriters
  ]

addWithMultipleWriters :: IO Int64
addWithMultipleWriters = do
    distrib <- new
    locks <- replicateM n newEmptyMVar
    mapM_ (forkIO . work distrib iters) locks
    mapM_ takeMVar locks
    total <- count <$> read distrib
    unless (fromIntegral total == n*iters) $
      error "Incorrect count!"
    pure total
  where
    n = 100
    iters = 100000

    work :: Distribution -> Int -> MVar () -> IO ()
    work !_ 0 !lock     = putMVar lock ()
    work distrib i lock = add distrib 1.0 >> work distrib (i - 1) lock
