module Initial where

import           Config
import           Elements
import           Mod
import           Resources

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
        (AcquirePaperclips
            (PaperclipsManually Initial.paperclipManuallyCost)
            (PaperclipsFromHelper noCost)
        )
        (Paperclips 0)
        (DurationPaperclips Instant)
    )
    (Element (AcquireEnergy (EnergyManually noCost))
             (Energy 20)
             (DurationEnergy Instant)
    )
    (Element (AcquireHelpers (HelpersManually helperCost))
             (Helpers 0)
             (DurationHelpers Instant)
    )
    (Element
        (AcquireTrees
            (TreesFromTreeSeeds treeCost)
            (TreeSeedCostPerTick
                (Cost (Paperclips 0)
                      (Energy 0)
                      (Helpers 0)
                      (Trees 0)
                      (TreeSeeds [])
                      (Water 2)
                      (Wood 0)
                )
            )
        )
        (Trees 0)
        (DurationTrees Instant)
    )
    (Element (AcquireTreeSeeds (BuyTreeSeeds Initial.treeSeedCost))
             (TreeSeeds (replicate 10 NotGrowing))
             (DurationTreeSeeds $ Ticks 20)
    )
    (Element (AcquireWater (WaterManually Initial.waterCost))
             (Water 100)
             (DurationWater Instant)
    )
    (Element (AcquireWood (WoodManually Initial.woodCost))
             (Wood 0)
             (DurationWood Instant)
    )

paperclipManuallyCost :: Cost Integer
paperclipManuallyCost = noCost

helperCost :: Cost Integer
helperCost = Cost (Paperclips 10)
                  (Energy 0)
                  (Helpers 0)
                  (Trees 0)
                  (TreeSeeds [])
                  (Water 0)
                  (Wood 0)

treeCost :: Cost Integer
treeCost = Cost (Paperclips 0)
                (Energy 0)
                (Helpers 0)
                (Trees 0)
                (TreeSeeds [GrowingDone])
                (Water 0)
                (Wood 0)

treeSeedCost :: Cost Integer
treeSeedCost = Cost (Paperclips 100)
                    (Energy 0)
                    (Helpers 0)
                    (Trees 0)
                    (TreeSeeds [])
                    (Water 0)
                    (Wood 0)

waterCost :: Cost Integer
waterCost = noCost

woodCost :: Cost Integer
woodCost = noCost

noCost :: Cost Integer
noCost = Cost (Paperclips 0)
              (Energy 0)
              (Helpers 0)
              (Trees 0)
              (TreeSeeds [])
              (Water 0)
              (Wood 0)

getInitialState :: MyState Integer
getInitialState = MyState Initial.config
                          []
                          []
                          Initial.researchAreas
                          Initial.resources
                          (Seconds 0)
                          (IsStarted False)
