{-# LANGUAGE OverloadedStrings #-}

module Initial where

import           Data.List.Zipper               ( empty )

import           Config
import           Elements
import           Resources
import           Seconds
import           Source
import           State

prices :: Prices Integer
prices = Prices (AdvancedHelperPrice $ Paperclips 5)

config :: Config Integer
config = Config (Constants (HelperInc (Helpers 1))) Initial.prices

researchAreas :: ResearchAreas Integer
researchAreas =
    ResearchAreas (ResearchComp (DurationAdvanced $ Ticks 20) NotResearched)

resources :: Resources Integer
resources =
    Resources Initial.elements (Storage (Paperclips 1000)) (WaterTank 100)

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
            (HelpersManually (CostEnergyPaperclips (Energy 10) (Paperclips 10)))
        )
        (Helpers 0)
        (DurationHelpers Instant)
    )
    (Element
        (AcquireTrees
            (TreesFromTreeSeeds (CostTreeSeeds (TreeSeeds [GrowingDone])))
            (TreeSeedCostPerTick (CostWater (Water 2)))
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


getInitialState :: State Integer
getInitialState = State Initial.config
                        []
                        []
                        Initial.events
                        Initial.researchAreas
                        Initial.resources
                        (Seconds 0)
                        (Snapshots empty)
                        (Source (SourceText "") (SourceStatus ""))
                        (Title "tickGame")
                        (IsStarted False)
