module DieHard where

import Prelude
import Language.Spectacle
  ( Action,
    Initial,
    Invariant,
    Terminate,
    always,
    defaultInteraction,
    define,
    modelCheck,
    plain,
    prime,
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

type Diehard =
  '[ "smallJug" # Int
   , "bigJug" # Int
   ]

initial :: Initial Diehard ()
initial = do
  #smallJug `define` pure 0
  #bigJug `define` pure 0

next :: Action Diehard Bool
next = foldr (\/) (pure True)
  [ fillSmall
  , fillBig
  , emptySmall
  , emptyBig
  , smallToBig
  , bigToSmall
  ]
    where
      fillSmall = #smallJug .= pure 3 >> pure True

      fillBig = #bigJug .= pure 5 >> pure True

      emptySmall = #smallJug .= pure 0 >> pure True

      emptyBig = #bigJug .= pure 0 >> pure True

      smallToBig = do
        smallJug <- plain #smallJug
        bigJug <- plain #bigJug
        #smallJug .= do
          bigJug' <- prime #bigJug
          pure (smallJug - (bigJug' - bigJug))
        #bigJug .= pure (min (bigJug + smallJug) 5)
        pure True

      bigToSmall = do
        smallJug <- plain #smallJug
        bigJug <- plain #bigJug
        #smallJug .= pure (min (bigJug + smallJug) 3)
        #bigJug .= do
          smallJug' <- prime #smallJug
          pure $ bigJug - (smallJug' - smallJug)
        pure True


formula :: Invariant Diehard Bool
formula = do
  always smallJugBounds
    /\ always bigJugBounds
    -- /\ always (not <$> solved)
  where
    smallJugBounds = do
      smallJug <- plain #smallJug
      pure (0 <= smallJug && smallJug <= 3)

    bigJugBounds = do
      bigJug <- plain #bigJug
      pure (0 <= bigJug && bigJug <= 5)

    -- solved = do
    --   bigJug <- plain #bigJug
    --   pure (bigJug /= 4)

termination :: Terminate Diehard Bool
termination = do
  bigJug <- plain #bigJug
  return (bigJug == 4)

check :: IO ()
check = do
  let spec :: Specification Diehard
      spec =
        Specification
          { initialAction = initial
          , nextAction = next
          , temporalFormula = formula
          , terminationFormula = Just termination
          , fairnessConstraint = strongFair
          }
  defaultInteraction (modelCheck spec)

-- $> check
