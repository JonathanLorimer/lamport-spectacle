{-# LANGUAGE OverloadedLabels, ExplicitNamespaces, DeriveAnyClass, DeriveGeneric #-}
module TransactionCommit where

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
    -- prime,
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

data Constants =
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

unsafeUpdate :: Int -> a -> [a] -> [a]
unsafeUpdate i e xs =
  let (init, _:tail) = splitAt i xs
   in init ++ e : tail

next :: Action TransactionCommit Bool
next = foldr (\/) (pure True)
  [ prepare
  , decide
  ]
    where
      -- Direct Actions
      prepare = do
        rms <- plain #resourceManagers
        exists (zip [0..] rms) $ \(idx,rm) -> do
          void . pure $ rm == Working
          (#resourceManagers .= pure (unsafeUpdate idx Prepared rms)) $> True
      decide = do
        rms <- plain #resourceManagers
        exists (zip [0..] rms) $ \(idx,rm) ->
             decideCommit idx rm rms
          /\ decideAbort idx rm rms


      -- Indirect Actions
      decideCommit :: Int -> ResourceManagerState -> [ResourceManagerState] -> Action TransactionCommit Bool
      decideCommit idx rm rms = do
        void . pure $ rm == Prepared
        void $ canCommit rms
        (#resourceManagers .= pure (unsafeUpdate idx Committed rms)) $> True

      decideAbort :: Int -> ResourceManagerState -> [ResourceManagerState] -> Action TransactionCommit Bool
      decideAbort idx rm rms = do
        void . pure $ rm == Working || rm == Prepared
        void $ notCommitted rms
        (#resourceManagers .= pure (unsafeUpdate idx Aborted rms)) $> True

      canCommit :: [ResourceManagerState] -> Action TransactionCommit Bool
      canCommit rms = do
        -- rms <- plain #resourceManagers
        forall rms $ \rm -> pure $ rm == Prepared || rm == Committed

      notCommitted :: [ResourceManagerState] -> Action TransactionCommit Bool
      notCommitted rms = do
        -- rms <- plain #resourceManagers
        forall rms $ \rm -> pure $ rm /= Committed



formula :: Invariant TransactionCommit Bool
formula =
  always (not <$> abortAndCommit)
    where
      abortAndCommit = do
        rms <- plain #resourceManagers
        pure (any (== Committed) rms && any (== Aborted) rms)

termination :: Terminate TransactionCommit Bool
termination = do
  rms <- plain #resourceManagers
  pure $ all (== Committed) rms || all (== Aborted) rms


check :: IO ()
check = do
  let constants = Constants { rmQuantity = 5 }
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
