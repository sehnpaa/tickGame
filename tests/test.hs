module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

main = defaultMain tests

defaultConfig :: Config
defaultConfig = Config (Prices (Paperclips 10))

state1 :: MyState
state1 = MyState defaultConfig [] [] 0 2 100 0 (IsStarted True)

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "first" $ assertEqual "" (MyState defaultConfig [] [] 4 2 100 1 (IsStarted True)) (nextTick state1)]