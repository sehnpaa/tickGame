module Main where

import Control.Lens
import Data.List (nub, permutations)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

import Lib

main :: IO ()
main = defaultMain tests

defaultConfig :: Config
defaultConfig = Config (Constants (HelperInc (Helpers 1))) (Durations $ TreeDuration 10) (Prices (AdvancedHelperPrice $ Paperclips 5) (HelperPrice $ Paperclips 10) (TreePrice 1))

state1 :: MyState
state1 = MyState defaultConfig [] [] (ResearchAreas (ResearchComp (Duration 20) NotResearched)) (Resources (Paperclips 0) (Helpers 2) (Storage 1000) (Trees 0) (TreeSeeds $ replicate 100 NotGrowing) (Wood 0)) (Seconds 0) (IsStarted True)

tests :: TestTree
tests = testGroup "Tests" [unitTests, qcTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "first" $ assertEqual "" (MyState defaultConfig [] [] (ResearchAreas (ResearchComp (Duration 20) NotResearched)) (Resources (Paperclips 2) (Helpers 2) (Storage 1000) (Trees 0) (TreeSeeds $ replicate 100 NotGrowing) (Wood 0)) (Seconds 1) (IsStarted True)) (nextTick state1)]

qcTests :: TestTree
qcTests = testGroup "QuickCheck tests"
  [ QC.testProperty "addSecond" $ QC.withMaxSuccess 1000 $ \state ->
      let before = view seconds state
          after = view seconds (addSecond state)
          in after == before + 1
  , QC.testProperty "Commutative features" $ QC.withMaxSuccess 1000 $ \state ->
      isCommutative state [addSecond, createPaperclip, helperWork] ]

isCommutative :: Eq a => a -> [a -> a] -> Bool
isCommutative empty = (\x -> length x == 1) . nub . map (foldr id empty) . permutations