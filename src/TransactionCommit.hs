{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedLists  #-}
module TransactionCommit where

import Prelude hiding (tail, init)
import Data.Hashable
import Data.Functor
import Data.Vector.Instances ()
import Data.Vector qualified as V
import Data.Vector (Vector)
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
  '[ "resourceManagers" # Vector ResourceManagerState
   ]

vec2assoc :: Vector a -> [(Int, a)]
vec2assoc = V.ifoldr (\i a b -> (i,a) : b) []

initial :: Constants -> Initial TransactionCommit ()
initial k = do
  #resourceManagers `define` pure ([1.. (rmQuantity k)] $> Working)

next :: Action TransactionCommit Bool
next = prepare \/ decide

    where
      -- Direct Actions
      prepare :: Action TransactionCommit Bool
      prepare = do
        rms <- plain #resourceManagers
        exists (vec2assoc rms) $ \(idx,rm) -> do
          void . pure $ rm == Working
          (#resourceManagers .= pure (V.update rms [(idx,Prepared)])) $> True
      decide = do
        rms <- plain #resourceManagers
        exists (vec2assoc rms) $ \(idx,rm) ->
             decideCommit idx rm rms
          /\ decideAbort idx rm rms


      -- Indirect Actions
      decideCommit :: Int -> ResourceManagerState -> Vector ResourceManagerState -> Action TransactionCommit Bool
      decideCommit idx rm rms = do
        void . pure $ rm == Prepared
        void $ canCommit rms
        (#resourceManagers .= pure (V.update rms [(idx,Committed)])) $> True

      decideAbort :: Int -> ResourceManagerState -> Vector ResourceManagerState -> Action TransactionCommit Bool
      decideAbort idx rm rms = do
        void . pure $ rm == Working || rm == Prepared
        void $ notCommitted rms
        (#resourceManagers .= pure (V.update rms [(idx,Aborted)])) $> True

      canCommit :: Vector ResourceManagerState -> Action TransactionCommit Bool
      canCommit rms = forall rms $ \rm -> pure $ rm == Prepared || rm == Committed

      notCommitted :: Vector ResourceManagerState -> Action TransactionCommit Bool
      notCommitted rms = forall rms $ \rm -> pure $ rm /= Committed



formula :: Invariant TransactionCommit Bool
formula =
  always (not <$> abortAndCommit)
    where
      abortAndCommit = do
        rms <- plain #resourceManagers
        pure (elem Committed rms && elem Aborted rms)

termination :: Terminate TransactionCommit Bool
termination = do
  rms <- plain #resourceManagers
  pure $ all (== Committed) rms || all (== Aborted) rms


check :: IO ()
check = do
  let constants = Constants { rmQuantity = 10 }
      spec :: Specification TransactionCommit
      spec =
        Specification
          { initialAction = initial constants
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Just termination
          , fairnessConstraint = strongFair
          }
  defaultInteraction (modelCheck spec)

-- $> check
