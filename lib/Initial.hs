{-# LANGUAGE OverloadedStrings #-}

module Initial where

import           Config
import           Elements
import           Resources
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
        (AcquireTreeSeeds (BuyTreeSeeds (CostPaperclips (Paperclips 100))))
        (TreeSeeds (replicate 10 NotGrowing))
        (DurationTreeSeeds $ Ticks 20)
    )
    (Element (AcquireWater (WaterManually NoCost))
             (Water 100)
             (DurationWater Instant)
    )
    (Element (AcquireWood (WoodManually NoCost)) (Wood 0) (DurationWood Instant)
    )

noCost :: Cost Integer
noCost = Cost (Paperclips 0)
              (Energy 0)
              (Helpers 0)
              (Trees 0)
              (TreeSeeds [])
              (Water 0)
              (Wood 0)

getInitialState :: State Integer
getInitialState = State
    Initial.config
    []
    []
    (Events
        (EventStart (ButtonData ("Start game", Enabled, Start)))
        (EventCreatePaperclip
            (ButtonData ("Create Paperclip", Enabled, CreatePaperclip))
        )
        (EventCreateHelper (ButtonData ("Create helper", Enabled, CreateHelper))
        )
        (EventPumpWater (ButtonData ("Pump water", Enabled, PumpWater)))
        (EventGenerateEnergy
            (ButtonData ("Generate energy", Enabled, GenerateEnergy))
        )
        (EventBuyASeed (ButtonData ("Buy a seed", Enabled, BuyASeed)))
        (EventPlantASeed (ButtonData ("Plant a seed", Enabled, PlantASeed)))
        (EventResearchAdvancedHelper
            (ButtonData
                ("Research advanced helper", Enabled, ResearchAdvancedHelper)
            )
        )
        (EventExitApplication (ButtonData ("Exit", Enabled, ExitApplication)))
    )
    Initial.researchAreas
    Initial.resources
    (Seconds 0)
    (Source "" "" Nothing)
    (Title "tickGame")
    (IsStarted False)
