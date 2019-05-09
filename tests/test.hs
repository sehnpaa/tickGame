module Main where

import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

main = defaultMain tests

defaultConfig :: Config
defaultConfig = Config (Prices (HelperPrice $ Paperclips 10))

state1 :: MyState
state1 = MyState defaultConfig [] [] 0 2 100 0 (IsStarted True)

tests :: TestTree
tests = testGroup "Tests" [unitTests, qcTests]

unitTests = testGroup "Unit tests"
  [ testCase "first" $ assertEqual "" (MyState defaultConfig [] [] 4 2 100 1 (IsStarted True)) (nextTick state1)]

qcTests = testGroup "QuickCheck tests"
  [ QC.testProperty "addSecond" $ QC.withMaxSuccess 1000 $ \state ->
      let before = view seconds state
          after = view seconds (addSecond state)
          in after == before + 1]