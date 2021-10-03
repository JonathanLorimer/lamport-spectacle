{-# LANGUAGE OverloadedLabels, ExplicitNamespaces, DeriveAnyClass, DeriveGeneric #-}
module PaxosCommit where

import Prelude
import Data.Hashable
import Data.Functor
import GHC.Generics
import GHC.Natural
import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    Terminate,
    always,
    defaultInteraction,
    define,
    exists,
    forall,
    modelCheck,
    plain,
    strongFair,
    (.=),
    (/\),
    (\/),
    type (#),
  )

import Language.Spectacle.Specification
  ( Specification
      ( Specification,
        fairnessConstraint,
        initialAction,
        nextAction,
        temporalFormula,
        terminationFormula
      ),
  )

newtype Constants =
  Constants
    { rmQuantity :: Natural
    }

data ResourceManagerState =
    Working
  | Prepared
  | Committed
  | Aborted
  deriving (Eq, Show, Generic, Hashable)

type TransactionCommit =
  '[ "resourceManagers" # [ResourceManagerState]
   ]

initial :: Constants -> Initial TransactionCommit ()
initial k = do
  #resourceManagers `define` pure ([1.. (rmQuantity k)] $> Working)

next :: Action TransactionCommit Bool
next = pure True



formula :: Invariant TransactionCommit Bool
formula = always True

termination :: Terminate TransactionCommit Bool
termination = pure True


spec :: Specification PaxosCommit
spec =
  Specification
    { initialAction = initial constants
    , nextAction = next
    , temporalFormula = formula
    , terminationFormula = Just termination
    , fairnessConstraint = strongFair
    }

check :: IO ()
check = defaultInteraction (modelCheck spec)
