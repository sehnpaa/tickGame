{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module GenerateEnergy where

import           Control.Lens                   ( makeLenses, view )
import           Control.Monad.Reader           ( runReader )
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           BusinessLogic
import           Elements

data MiniState a = MiniState { _miniStateEnergy :: Energy a }
makeLenses ''MiniState

instance Show (MiniState Integer) where
  show (MiniState (Energy n)) = show n

instance Arbitrary (MiniState Integer) where
  arbitrary = MiniState . Energy <$> arbitrary

instance HasEnergy (MiniState a) a where
  energy = miniStateEnergy

generateEnergyProps :: TestTree
generateEnergyProps = testGroup "generateEnergy"
  [ testProperty "more energy" $
      \state' -> (view miniStateEnergy state' :: Energy Integer) < runReader generateEnergy state'
  ]