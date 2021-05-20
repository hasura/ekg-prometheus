{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Generic.Random (genericArbitrary, uniform)
import GHC.Generics
import System.Exit
import Test.Hspec
import qualified Test.Hspec.SmallCheck as SC
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck as SC
import qualified Test.SmallCheck.Drivers as SC
import qualified Test.SmallCheck.Series as SC

import System.Metrics.Internal

------------------------------------------------------------------------
-- * Restricted state operations
--
-- We define subsets of the state types in order to restrict the space
-- of inputs to property tests. In particular, it is important to
-- restrict the identifiers to a small set so that we can reliably
-- obtain identifier collisons for testing purposes.
--
-- These subsets are realized by "rendering" functions into the original
-- sets.

-- | All possible state operations over a manually-specified set of
-- inputs.
data TestOperation
  = TestRegister TestIdentifier
  | TestRegisterGroup TestIdentifierGroup
  | TestDeregister TestIdentifier
  | TestDeregisterByName TestName
  deriving (Generic, Show)

-- | An enumeration of manually-specified metric identifiers.
data TestIdentifier = TestIdentifier TestName TestTagSet
  deriving (Generic, Show)

-- | An enumeration of manually-specified metric names.
data TestName = NameA | NameB
  deriving (Generic, Show)

-- | An enumeration of manually-specified sets of tags.
data TestTagSet = TagSetA | TagSetB
  deriving (Generic, Show)

-- | An enumeration of manually-specified groups of identifiers.
data TestIdentifierGroup
  = IdGroup0
  | IdGroup1
  | IdGroup2A
  | IdGroup2B
  | IdGroup2C
  deriving (Generic, Show)

-- Smallcheck instances
instance (Monad m) => SC.Serial m TestOperation
instance (Monad m) => SC.Serial m TestIdentifier
instance (Monad m) => SC.Serial m TestName
instance (Monad m) => SC.Serial m TestTagSet
instance (Monad m) => SC.Serial m TestIdentifierGroup

-- Quickcheck instances
instance QC.Arbitrary TestOperation where
  arbitrary = genericArbitrary uniform
instance QC.Arbitrary TestIdentifier where
  arbitrary = genericArbitrary uniform
instance QC.Arbitrary TestName where
  arbitrary = genericArbitrary uniform
instance QC.Arbitrary TestTagSet where
  arbitrary = genericArbitrary uniform
instance QC.Arbitrary TestIdentifierGroup where
  arbitrary = genericArbitrary uniform

-- ** Rendering functions
--
-- Here we generate the real inputs on which to run our tests. Note that
-- whenever sampling actions are required these functions use dummy
-- sampling actions, making these inputs unsuitable for testing the
-- sampling logic.
--
-- Note: In general, the inputs generated by these functions have not
-- been carefully considered and could likely be improved.

renderOperation :: TestOperation -> State -> State
renderOperation testOp = case testOp of
  TestDeregister id' -> deregister (renderIdentifier id')
  TestRegister id' -> register (renderIdentifier id') (CounterS (pure 0))
  TestRegisterGroup idGroup ->
    registerGroup (renderIdentifierGroup idGroup) (pure 0)
  TestDeregisterByName name -> deregisterByName (renderName name)

renderIdentifier :: TestIdentifier -> Identifier
renderIdentifier (TestIdentifier name tagSet) =
  Identifier (renderName name) (renderTagSet tagSet)

renderName :: TestName -> T.Text
renderName NameA = "a"
renderName NameB = "b"

renderTagSet :: TestTagSet -> M.HashMap T.Text T.Text
renderTagSet TagSetA = M.fromList [("key", "a")]
renderTagSet TagSetB = M.fromList [("key", "b")]

renderIdentifierGroup ::
  TestIdentifierGroup -> M.HashMap Identifier (a -> Value)
renderIdentifierGroup testGroup =
  render_ $ case testGroup of
    IdGroup0 -> mempty
    IdGroup1 -> idAA
    IdGroup2A -> idAA <> idAB
    IdGroup2B -> idAA <> idBA
    IdGroup2C -> idBA <> idBB
  where
    idAA = [ TestIdentifier NameA TagSetA ]
    idAB = [ TestIdentifier NameA TagSetB ]
    idBA = [ TestIdentifier NameB TagSetA ]
    idBB = [ TestIdentifier NameB TagSetB ]

    render_ :: [TestIdentifier] -> M.HashMap Identifier (a -> Value)
    render_ =
        M.fromList
      . map (\id' -> (renderIdentifier id', const (Counter 0)))

------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "A sequence of operations on the internal state" $ do
    let verifyOps :: [TestOperation] -> Bool
        verifyOps ops =
          verifyState $ foldl' (flip renderOperation) initialState ops
    it "preserves internal consistency (smallcheck)" $
      -- A depth of 5 yields sequences of operations up to length 3.
      -- The test takes too long if we go any deeper.
      SC.property $ SC.changeDepth (const 5) $ SC.forAll verifyOps
    it "preserves internal consistency (quickcheck)" $
      QC.property verifyOps