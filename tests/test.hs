module Main where

import           Data.List                      ( nub
                                                , permutations
                                                )
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib
import           View

main :: IO ()
main = defaultMain tests

defaultConfig :: Config Integer
defaultConfig = Config
  (Constants (HelperInc (Helpers 1)))
  (Prices (AdvancedHelperPrice $ Paperclips 5) (ProgPrice 2) (TreePrice 1))

state1 :: MyState Integer
state1 = MyState
  defaultConfig
  []
  []
  (ResearchAreas (ResearchComp (DurationAdvanced $ Ticks 20) NotResearched))
  Main.resources
  (Seconds 0)
  (IsStarted True)

resources :: Resources Integer
resources = Resources Main.elements (Storage (Paperclips 1000)) (WaterTank 100)

elements :: Elements Integer
elements = Elements
  (Element
    (AcquirePaperclips (PaperclipsManually Main.paperclipManuallyCost)
                       (PaperclipsFromHelper noCost)
    )
    (Paperclips 0)
    (DurationPaperclips Instant)
  )
  (Element (AcquireHelpers (HelpersManually helperCost))
           (Helpers 0)
           (DurationHelpers Instant)
  )
  (Element (AcquireTrees (TreesFromTreeSeeds treeCost))
           (Trees 0)
           (DurationTrees Instant)
  )
  (Element (AcquireTreeSeeds (BuyTreeSeeds Main.treeSeedCost))
           (TreeSeeds (replicate 10 NotGrowing))
           (DurationTreeSeeds $ Ticks 20)
  )

  (Element (AcquireWater (WaterManually noCost))
           (Water 100)
           (DurationWater Instant)
  )
  (Element (AcquireWood (WoodManually noCost)) (Wood 0) (DurationWood Instant))

paperclipManuallyCost :: Cost Integer
paperclipManuallyCost = noCost

helperCost :: Cost Integer
helperCost =
  Cost (Paperclips 10) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

treeCost :: Cost Integer
treeCost = noCost

treeSeedCost :: Cost Integer
treeSeedCost = noCost

noCost :: Cost Integer
noCost =
  Cost (Paperclips 0) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "first second"
      $ assertEqual "" (Seconds 1) (viewSeconds $ nextTick state1)
  ]

isCommutative :: Eq a => a -> [a -> a] -> Bool
isCommutative empty =
  (\x -> length x == 1) . nub . map (foldr id empty) . permutations
