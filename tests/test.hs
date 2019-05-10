module Main where

import Control.Lens
import Data.List (nub, permutations)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

main = defaultMain tests

defaultConfig :: Config
defaultConfig = Config (Prices (HelperPrice $ Paperclips 10))

state1 :: MyState
state1 = MyState defaultConfig [] [] (Resources 0 2 0 100) 0 (IsStarted True)

tests :: TestTree
tests = testGroup "Tests" [unitTests, qcTests]

unitTests = testGroup "Unit tests"
  [ testCase "first" $ assertEqual "" (MyState defaultConfig [] [] (Resources 4 2 0 100) 1 (IsStarted True)) (nextTick state1)]

qcTests = testGroup "QuickCheck tests"
  [ QC.testProperty "addSecond" $ QC.withMaxSuccess 1000 $ \state ->
      let before = view seconds state
          after = view seconds (addSecond state)
          in after == before + 1
  , QC.testProperty "Commutative features" $ QC.withMaxSuccess 1000 $ \state ->
      isCommutative state [addSecond, createPC, helperWork, plantASeed] ]

isCommutative :: Eq a => a -> [a -> a] -> Bool
isCommutative empty = (\x -> length x == 1) . nub . map (foldr id empty) . permutations