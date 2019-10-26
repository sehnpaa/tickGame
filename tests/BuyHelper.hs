{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module BuyHelper where

import           Control.Lens                   ( makeLenses
                                                , view
                                                )
import           Control.Monad.Reader           ( runReader )
import           Data.Text
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           BusinessLogic
import           Elements
import           Seconds


data MiniState a = MiniState
  { _miniStateEnergy :: Energy a
  , _miniStateCostEnergyPaperclips :: CostEnergyPaperclips a
  , _miniStateEnergyErrorMessage :: EnergyErrorMessage
  , _miniStateHelpers :: Helpers a
  , _miniStatePaperclips :: Paperclips a
  , _miniStatePaperclipsErrorMessage :: PaperclipsErrorMessage
  , _miniStateSeconds :: Seconds a }
makeLenses ''MiniState

instance Show (MiniState Integer) where
  show = const ""

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary (MiniState Integer) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    g <- arbitrary
    h <- arbitrary
    return $ MiniState (Energy a)
                       (CostEnergyPaperclips (Energy b) (Paperclips c))
                       (EnergyErrorMessage d)
                       (Helpers e)
                       (Paperclips f)
                       (PaperclipsErrorMessage g)
                       (Seconds h)

instance HasCostEnergyPaperclips (MiniState a) a where
  costEnergyPaperclips = miniStateCostEnergyPaperclips

instance HasEnergy (MiniState a) a where
  energy = miniStateEnergy

instance HasEnergyErrorMessage (MiniState a) where
  energyErrorMessage = miniStateEnergyErrorMessage

instance HasHelpers (MiniState a) a where
  helpers = miniStateHelpers

instance HasPaperclips (MiniState a) a where
  paperclips = miniStatePaperclips

instance HasPaperclipsErrorMessage (MiniState a) where
  paperclipsErrorMessage = miniStatePaperclipsErrorMessage

instance HasSeconds (MiniState a) a where
  seconds = miniStateSeconds

buyHelperProps :: TestTree
buyHelperProps = testGroup
  "buyHelper"
  [ testProperty "" $ withMaxSuccess 10000 $ \state ->
      case runReader buyHelper (state :: MiniState Integer) of
        Right (h', _, _) -> moreHelpers h' state
        Left  _            -> True
  ]

moreHelpers :: Ord a => Helpers a -> MiniState a -> Bool
moreHelpers newHelpers oldState = newHelpers > view miniStateHelpers oldState