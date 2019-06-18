module Main where

import Data.List (nub, permutations)
import Test.Tasty
import Test.Tasty.HUnit

import Lib
import View

main :: IO ()
main = defaultMain tests

defaultConfig :: Config
defaultConfig = Config (Constants (HelperInc (Helpers 1))) (Durations $ TreeDuration 10) (Prices (AdvancedHelperPrice $ Paperclips 5) (HelperPrice $ Paperclips 10) (ProgPrice 2) (TreePrice 1) (TreeSeedPrice $ Paperclips 100))

state1 :: MyState
state1 = MyState defaultConfig [] [] (ResearchAreas (ResearchComp (Duration 20) NotResearched)) Main.resources (Seconds 0) (IsStarted True)

resources :: Resources
resources = Resources Main.elements (Storage (Paperclips 1000)) (WaterTank 100)

elements :: Elements
elements = Elements (Paperclips 0) (Helpers 2) (Trees 0) (TreeSeeds $ replicate 100 NotGrowing) (Water 0) (Wood 0)

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "first second" $ assertEqual "" 1 (viewSeconds $ nextTick state1)]

isCommutative :: Eq a => a -> [a -> a] -> Bool
isCommutative empty = (\x -> length x == 1) . nub . map (foldr id empty) . permutations