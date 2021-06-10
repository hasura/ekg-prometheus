module System.Metrics.ThreadId
    ( myCapability
    ) where

import qualified Control.Concurrent as Concurrent

myCapability :: IO Int
myCapability =
  fst <$> (Concurrent.threadCapability =<< Concurrent.myThreadId)
