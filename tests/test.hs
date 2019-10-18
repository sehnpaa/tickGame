{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                      ( nub
                                                , permutations
                                                )
import qualified Data.List.Zipper
import           Data.Text                      ( empty )
import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib
import           View

main :: IO ()
main = defaultMain tests

defaultConfig :: Config Integer
defaultConfig = Config (Constants (HelperInc (Helpers 1)))
                       (Prices (AdvancedHelperPrice $ Paperclips 5))

events :: Events
events = Events
  (EventStart
    (ButtonData (ButtonTitle "Start game")
                (ButtonStatus Enabled)
                (ButtonEvent Start)
    )
  )
  (EventCreatePaperclip
    (ButtonData (ButtonTitle "Create Paperclip")
                (ButtonStatus Enabled)
                (ButtonEvent CreatePaperclip)
    )
  )
  (EventCreateHelper
    (ButtonData (ButtonTitle "Create helper")
                (ButtonStatus Enabled)
                (ButtonEvent CreateHelper)
    )
  )
  (EventExtendStorage
    (ButtonData (ButtonTitle "Extend storage")
                (ButtonStatus Enabled)
                (ButtonEvent ExtendStorage)
    )
  )
  (EventPumpWater
    (ButtonData (ButtonTitle "Pump water")
                (ButtonStatus Enabled)
                (ButtonEvent PumpWater)
    )
  )
  (EventGenerateEnergy
    (ButtonData (ButtonTitle "Generate energy")
                (ButtonStatus Enabled)
                (ButtonEvent GenerateEnergy)
    )
  )
  (EventBuyASeed
    (ButtonData (ButtonTitle "Buy a seed")
                (ButtonStatus Enabled)
                (ButtonEvent BuyASeed)
    )
  )
  (EventPlantASeed
    (ButtonData (ButtonTitle "Plant a seed")
                (ButtonStatus Enabled)
                (ButtonEvent PlantASeed)
    )
  )
  (EventResearchAdvancedHelper
    (ButtonData (ButtonTitle "Research advanced helper")
                (ButtonStatus Enabled)
                (ButtonEvent ResearchAdvancedHelper)
    )
  )
  (EventPreviousSnapshot
    (ButtonData (ButtonTitle "Previous snapshot")
                (ButtonStatus Enabled)
                (ButtonEvent PreviousSnapshot)
    )
  )
  (EventNextSnapshot
    (ButtonData (ButtonTitle "Next snapshot")
                (ButtonStatus Enabled)
                (ButtonEvent NextSnapshot)
    )
  )
  (EventApplySnapshot
    (ButtonData (ButtonTitle "Apply snapshot")
                (ButtonStatus Enabled)
                (ButtonEvent ApplySnapshot)
    )
  )
  (EventExitApplication
    (ButtonData (ButtonTitle "Exit")
                (ButtonStatus Enabled)
                (ButtonEvent ExitApplication)
    )
  )

state1 :: State Integer
state1 = State
  defaultConfig
  []
  []
  Main.events
  (ResearchAreas
    (ResearchComp (DurationAdvanced $ Ticks 20)
                  NotResearched
                  "Not enough paperclips."
                  "Already in progress."
                  "Already done."
    )
  )
  Main.resources
  (Seconds 0)
  (Snapshots Data.List.Zipper.empty)
  (Source (SourceText empty) (SourceStatus empty))
  (Title "tickGame")
  (IsStarted True)

resources :: Resources Integer
resources = Resources Main.elements (Storage (Paperclips 1000)) (WaterTank 100)

elements :: Elements Integer
elements = Elements
  (Element
    (AcquirePaperclips (PaperclipsManually NoCost) (PaperclipsFromHelper NoCost)
    )
    (Paperclips 0)
    (DurationPaperclips Instant)
  )
  (Element (AcquireEnergy (EnergyManually NoCost))
           (Energy 20)
           (DurationEnergy Instant)
  )
  (Element
    (AcquireHelpers
      (HelpersManually helperCost "Not enough energy." "Not enough paperclips.")
    )
    (Helpers 0)
    (DurationHelpers Instant)
  )
  (Element
    (AcquireStorage (StorageManually (CostWood (Wood 1)) "Not enough wood."))
    (Storage 1000)
    (DurationStorage Instant)
  )
  (Element
    (AcquireTrees
      (TreesFromTreeSeeds (CostTreeSeeds (TreeSeeds [GrowingDone])))
      (TreeSeedCostPerTick (CostWater (Water 2))
                           "Not enough water for the seeds."
      )
    )
    (Trees 0)
    (DurationTrees Instant)
  )
  (Element
    (AcquireTreeSeeds
      (BuyTreeSeeds (CostPaperclips (Paperclips 10)) "Not enough paperclips.")
    )
    (TreeSeeds (replicate 10 NotGrowing))
    (DurationTreeSeeds $ Ticks 20)
  )

  (Element (AcquireWater (WaterManually NoCost))
           (Water 100)
           (DurationWater Instant)
  )
  (Element (AcquireWood (WoodManually NoCost)) (Wood 0) (DurationWood Instant))

helperCost :: CostEnergyPaperclips Integer
helperCost = CostEnergyPaperclips (Energy 10) (Paperclips 10)

noCost :: Cost Integer
noCost = Cost (Paperclips 0)
              (Energy 0)
              (Helpers 0)
              (Trees 0)
              (TreeSeeds [])
              (Water 0)
              (Wood 0)

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "first second"
      $ assertEqual "" (Seconds 1) (viewSeconds $ nextTick state1)
  ]

isCommutative :: Eq a => a -> [a -> a] -> Bool
isCommutative zero =
  (\x -> length x == 1) . nub . map (foldr id zero) . permutations
