{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Polytest where

import           Control.Lens                   ( makeClassy )
import           Test.Tasty
import           Test.Tasty.HUnit

import           Elements
import           BusinessLogic

data MiniState a = MiniState { _miniStateEnergy :: Energy a }
makeClassy ''MiniState

instance HasEnergy (MiniState a) a where
  energy = miniStateEnergy

x :: Energy Integer
x = generateEnergy miniStateEnergy (MiniState (Energy 55))

polyTests :: TestTree
polyTests = testGroup
  "Poly tests"
  [ testCase "generateEnergy" $ assertEqual
      ""
      (generateEnergy miniStateEnergy (MiniState (Energy 55)))
      (Energy (56 :: Integer))
  ]
