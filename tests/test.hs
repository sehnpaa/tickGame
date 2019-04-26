module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

main = defaultMain tests

state1 :: MyState
state1 = MyState [] [] 0 2 100 0 True

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "first" $ assertEqual "" (MyState [] [] 4 2 100 1 True) (nextTick state1)]