{-# LANGUAGE ExistentialQuantification #-}

module StateStep where

import           Control.Lens                   ( over )
import           Unsafe.Coerce                  ( unsafeCoerce )

import qualified PathedBusinessLogic           as PBL
import           State
import           Utils

data StateStep a = forall b. StateStep
          { sa :: a -> b
          , sc :: a -> b -> a }

instance Semigroup (StateStep a) where
  (StateStep ab aba) <> (StateStep ab' aba') = StateStep
    (\a -> ab' $ first a)
    (\a b -> aba' (first a) b)
    where first a = aba a (ab a)

instance Monoid (StateStep a) where
  -- This is safe since we throw away the result of unsafeCoerce with const
  mempty = StateStep unsafeCoerce const

runS :: StateStep a -> a -> a
runS (StateStep x y) a = y a (x a)

applyMultiple :: Foldable t => state -> t (StateStep state) -> state
applyMultiple = foldr runS

addActions :: State a -> [Action a] -> State a
addActions state newActions = over actions (\as -> newActions ++ as) state

-- Convenient alternative to the StateStep constructor
toStateStep :: (State a -> [Action a]) -> StateStep (State a)
toStateStep = flip StateStep addActions

--

buyASeed :: (Num a, Ord a, Show a) => StateStep (State a)
buyASeed = toStateStep
  ((withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])) . PBL.buyASeed)

buyHelper :: (Enum a, Num a, Ord a, Show a) => StateStep (State a)
buyHelper = toStateStep
  ( (withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : []))
  . PBL.buyHelper
  )

createPaperclip :: (Enum a, Ord a) => StateStep (State a)
createPaperclip = toStateStep ((\p -> SetP p : []) . PBL.createPaperclip)

generateEnergy :: (Enum a, Num a, Ord a, Show a) => StateStep (State a)
generateEnergy = toStateStep ((\e -> SetEnergy e : []) . PBL.generateEnergy)

helperWork :: (Num a, Ord a) => StateStep (State a)
helperWork = toStateStep ((singleton . SetP) . PBL.helperWork)

plantASeed :: (Num a, Ord a, Show a) => StateStep (State a)
plantASeed =
  toStateStep ((withError SetE (\s -> SetTreeSeeds s : [])) . PBL.plantASeed)

pumpWater :: (Enum a, Num a, Ord a) => StateStep (State a)
pumpWater = toStateStep ((\w -> SetWater w : []) . PBL.pumpWater)

researchAdvancedHelper :: (Num a, Ord a, Show a) => StateStep (State a)
researchAdvancedHelper = toStateStep
  ( (withError SetE (\(p, r) -> SetP p : SetR r : []))
  . PBL.researchAdvancedHelper
  )

researchWork :: (Eq a, Num a) => StateStep (State a)
researchWork = toStateStep
  ( (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
  . PBL.researchWork
  )

seedWork :: (Num a, Ord a, Show a) => StateStep (State a)
seedWork = toStateStep
  ( (withExtendedError
      SetE
      (\p -> SetProgs p : [])
      (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
    )
  . PBL.seedWork
  )
