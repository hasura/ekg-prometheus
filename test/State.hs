{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module State
  ( tests
  ) where

import Control.Applicative
import Data.Foldable (asum)
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import Test.HUnit hiding (State)
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Series as SC

import System.Metrics.Internal.State

------------------------------------------------------------------------
-- * Testing state operations
--
-- We generate state operations (e.g. register, deregister) on a
-- restricted space of identifiers so that the operations are more
-- likely to interact.

-- ** Restricted inputs

names :: [T.Text]
names = ["a", "b"]

tagSets :: [M.HashMap T.Text T.Text]
tagSets = [M.singleton "k" "v", M.singleton "k" "w"]

identifiers :: [Identifier]
identifiers = Identifier <$> names <*> tagSets

identifierGroups :: [[Identifier]]
identifierGroups =
  [ []
  , [a]
  , [a, b], [a, c], [b, c]
  , [a, b, c]
  ]
  where
    (a:b:c:_) = identifiers

samplingGroups :: [M.HashMap Identifier (() -> Value)]
samplingGroups = map (M.fromList . map (, sample)) identifierGroups
  where sample = const (Counter 0)

-- ** State operation representation

-- | A representation of all state operations, ignoring sampling
-- actions.
data TestStateOp
  = Register Identifier
  | RegisterGroup (M.HashMap Identifier (() -> Value))
  | Deregister Identifier
  | DeregisterByName T.Text

-- | Realize the state operations (using phony sampling actions).
runTestStateOp :: TestStateOp -> State -> State
runTestStateOp op = case op of
  Register identifier -> fst . register identifier (CounterS (pure 0))
  RegisterGroup group -> fst . registerGroup group (pure ())
  Deregister identifier -> deregister identifier
  DeregisterByName name -> deregisterByName name

instance Show TestStateOp where
  show (Register id') = "Register (" ++ show id' ++ ")"
  show (RegisterGroup idGroup) = "RegisterGroup " ++ show (M.keys idGroup)
  show (Deregister id') = "Deregister (" ++ show id' ++ ")"
  show (DeregisterByName name) = "DeregisterByName " ++ show name

instance (Monad m) => SC.Serial m TestStateOp where
  series = asum
    [ Register <$> choose identifiers
    , RegisterGroup <$> choose samplingGroups
    , Deregister <$> choose identifiers
    , DeregisterByName <$> choose names
    ]
    where
      choose :: (Alternative f) => [a] -> f a
      choose = foldr ((<|>) . pure) empty

instance QC.Arbitrary TestStateOp where
  -- | Frequencies are biased towards registration but are otherwise
  -- arbitrary
  arbitrary = QC.frequency
    [ (4, Register <$> QC.elements identifiers)
    , (4, RegisterGroup <$> QC.elements samplingGroups)
    , (2, Deregister <$> QC.elements identifiers)
    , (1, DeregisterByName <$> QC.elements names)
    ]

------------------------------------------------------------------------
-- * Deregistration handle smoke tests

test_handleEffectiveness :: IO ()
test_handleEffectiveness = do
  let identifier = Identifier "a" mempty
      (state1, handle) =
        register identifier (CounterS (pure 0)) initialState
      state2 = deregisterByHandle handle state1
  sampledState2 <- sampleState state2
  assertBool "Deregistration by handle does not remove the metric" $
        sampledStateMetrics sampledState2 == mempty
    &&  sampledStateGroups sampledState2 == mempty

test_handleSpecificity :: IO ()
test_handleSpecificity = do
  let identifier = Identifier "a" mempty
      (state1, handle) =
        register identifier (CounterS (pure 0)) initialState
      (state2, _) =
        register identifier (CounterS (pure 1)) state1
      state3 = deregisterByHandle handle state2
  sampledState2 <- sampleState state2
  sampledState3 <- sampleState state3
  assertEqual "Deregistration of a stale handle is not a no-op"
    sampledState2 sampledState3

------------------------------------------------------------------------

tests :: Spec
tests = do
  describe "A sequence of operations on the internal state" $ do
    let verifyOps :: [TestStateOp] -> Bool
        verifyOps ops =
          verifyState $ foldl' (flip runTestStateOp) initialState ops
    it "preserves internal consistency (smallcheck)" $
      -- A depth of 4 yields sequences of operations up to length 3.
      -- The test takes too long if we go any deeper.
      SC.property $ SC.changeDepth (const 4) $ SC.forAll verifyOps
    it "preserves internal consistency (quickcheck)" $
      QC.property verifyOps
  describe "Deregistration by a handle" $ do
    it "removes the intended metric"
      test_handleEffectiveness
    it "has no effect if the metric is already deregistered"
      test_handleSpecificity
