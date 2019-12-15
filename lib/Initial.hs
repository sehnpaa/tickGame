{-# LANGUAGE OverloadedStrings #-}

module Initial where

import           Data.List.Zipper               ( empty )
import           Tradeoff

import           Config
import           Elements
import           Resources
import           Seconds
import           Source
import           State

prices :: Prices Integer
prices = Prices (Tradeoff [] [(Resource "paperclips" (Just 0) Nothing, 25)] [] [])

config :: Config Integer
config = Config (Constants (HelperInc 1)) Initial.prices

researchAreas :: ResearchAreas Integer
researchAreas = ResearchAreas
    (ResearchComp (DurationAdvanced $ Ticks 20)
                  NotResearched
                  "Not enough paperclips."
                  "Already in progress."
                  "Already done."
    )

resources :: Resources Integer
resources =
    Resources Initial.elements (StorageOfPaperclips 1000) (WaterTank 100)

elements :: Elements Integer
elements = Elements
    (Element
        (AcquirePaperclips (PaperclipsManually NoCost)
                           (PaperclipsFromHelper NoCost)
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
            (HelpersManually
                (CostEnergyPaperclips (Energy 10) (Paperclips 10))
                (EnergyErrorMessage "Not enough energy.")
                (PaperclipsErrorMessage "Not enough paperclips.")
            )
        )
        (Helpers 0)
        (DurationHelpers Instant)
    )
    (Element
        (AcquireStorage (StorageManually (CostWood (Wood 1)) "Not enough wood.")
        )
        (StorageOfPaperclips 1000)
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
            (BuyTreeSeeds (CostPaperclips (Paperclips 100))
                          "Not enough paperclips."
            )
        )
        (TreeSeeds (replicate 10 NotGrowing))
        (DurationTreeSeeds $ Ticks 20)
    )
    (Element (AcquireWater (WaterManually NoCost))
             (Water 100)
             (DurationWater Instant)
    )
    (Element (AcquireWood (WoodManually NoCost)) (Wood 2) (DurationWood Instant)
    )

noCost :: Cost Integer
noCost = Cost (Paperclips 0)
              (Energy 0)
              (Helpers 0)
              (Trees 0)
              (TreeSeeds [])
              (Water 0)
              (Wood 0)

events :: Events
events = Events
    (EventStart
        (ButtonData (ButtonTitle "Start game")
                    (ButtonStatus Enabled)
                    (ButtonEvent Start)
                    (ButtonDisplayStatus (ShowStatic "Start") (ShowStatic "Pause"))
        )
    )
    (EventCreatePaperclip
        (ButtonData (ButtonTitle "Create Paperclip")
                    (ButtonStatus Enabled)
                    (ButtonEvent CreatePaperclip)
                    (ButtonDisplayStatus ShowCost (ShowStatic "Not available"))
        )
    )
    (EventCreateHelper
        (ButtonData (ButtonTitle "Create helper")
                    (ButtonStatus Enabled)
                    (ButtonEvent CreateHelper)
                    (ButtonDisplayStatus ShowCost (ShowStatic "Not available"))
        )
    )
    (EventExtendStorage
        (ButtonData (ButtonTitle "Extend storage")
                    (ButtonStatus Enabled)
                    (ButtonEvent ExtendStorage)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventPumpWater
        (ButtonData (ButtonTitle "Pump water")
                    (ButtonStatus Enabled)
                    (ButtonEvent PumpWater)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventGenerateEnergy
        (ButtonData (ButtonTitle "Generate energy")
                    (ButtonStatus Enabled)
                    (ButtonEvent GenerateEnergy)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventBuyASeed
        (ButtonData (ButtonTitle "Buy a seed")
                    (ButtonStatus Enabled)
                    (ButtonEvent BuyASeed)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventPlantASeed
        (ButtonData (ButtonTitle "Plant a seed")
                    (ButtonStatus Enabled)
                    (ButtonEvent PlantASeed)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventResearchAdvancedHelper
        (ButtonData (ButtonTitle "Research advanced helper")
                    (ButtonStatus Enabled)
                    (ButtonEvent ResearchAdvancedHelper)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventPreviousSnapshot
        (ButtonData (ButtonTitle "Previous snapshot")
                    (ButtonStatus Enabled)
                    (ButtonEvent PreviousSnapshot)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventNextSnapshot
        (ButtonData (ButtonTitle "Next snapshot")
                    (ButtonStatus Enabled)
                    (ButtonEvent NextSnapshot)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventApplySnapshot
        (ButtonData (ButtonTitle "Apply snapshot")
                    (ButtonStatus Enabled)
                    (ButtonEvent ApplySnapshot)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )
    (EventExitApplication
        (ButtonData (ButtonTitle "Exit")
                    (ButtonStatus Enabled)
                    (ButtonEvent ExitApplication)
                    (ButtonDisplayStatus (ShowStatic "x") (ShowStatic "x"))
        )
    )


getInitialState :: State Integer
getInitialState = State Initial.config
                        (Actions [])
                        []
                        Initial.events
                        Initial.researchAreas
                        Initial.resources
                        (Seconds 0)
                        (Snapshots empty)
                        (Source (SourceText "") (SourceStatus ""))
                        (Title "tickGame")
                        (IsStarted False)
