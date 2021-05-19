{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.List (foldl')
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import GHC.Generics
import System.Exit
import Test.SmallCheck
import Test.SmallCheck.Drivers
import Test.SmallCheck.Series

import System.Metrics.Internal

------------------------------------------------------------------------
-- * Operations

data TestOperation
  = TestDeregister TestIdentifier
  | TestRegister TestIdentifier
  | TestRegisterGroup TestIdentifierGroup
  | TestDeregisterByName TestName
  deriving (Generic, Show)

data TestIdentifier = TestIdentifier TestName TestTagSet
  deriving (Generic, Show)

data TestName = NameA | NameB
  deriving (Generic, Show)

data TestTagSet = TagSetA | TagSetB
  deriving (Generic, Show)

data TestIdentifierGroup
  = IdGroup0
  | IdGroup1
  | IdGroup2A
  | IdGroup2B
  | IdGroup2C
  deriving (Generic, Show)

instance (Monad m) => Serial m TestOperation
instance (Monad m) => Serial m TestIdentifier
instance (Monad m) => Serial m TestName
instance (Monad m) => Serial m TestTagSet
instance (Monad m) => Serial m TestIdentifierGroup

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
main = do
  mPropFailure <-
    -- depth=5 gives sequences of operations up to length 3
    smallCheckM 5 $ forAll verifyOps
  case mPropFailure of
    Nothing -> void exitSuccess
    Just propFailure -> do
      putStrLn "Test failed: Validate internal consistency"
      print propFailure
      void exitFailure
  where
    verifyOps :: [TestOperation] -> Bool
    verifyOps ops =
      verifyState $ foldl' (flip renderOperation) initialState ops
