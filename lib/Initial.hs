module Initial where

import           Config
import           Elements
import           Mod
import           Resources

prices :: Prices Integer
prices =
    Prices (AdvancedHelperPrice $ Paperclips 5) (ProgPrice 2) (TreePrice 1)

config :: Config
config = Config (Constants (HelperInc (Helpers 1))) prices

researchAreas :: ResearchAreas
-- researchAreas = ResearchAreas (ResearchComp (Duration 10) NotResearched)
researchAreas = ResearchAreas (ResearchComp (DurationAdvanced $ Ticks 20) NotResearched)

resources :: Resources
resources = Resources elements (Storage (Paperclips 1000)) (WaterTank 100)

elements :: Elements
elements = Elements
    (Element
        (AcquirePaperclips (PaperclipsManually Initial.paperclipCost)
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
    (Element (AcquireTreeSeeds (BuyTreeSeeds Initial.treeSeedCost))
             (TreeSeeds (replicate 10 NotGrowing))
             (DurationTreeSeeds $ Ticks 20)
    )
    (Element (AcquireWater (WaterManually Initial.waterCost))
             (Water 100)
             (DurationWater Instant)
    )
    (Element (AcquireWood (WoodManually woodCost))
             (Wood 0)
             (DurationWood Instant)
    )

paperclipCost :: Cost Integer
paperclipCost = noCost

helperCost :: Cost Integer
helperCost =
    Cost (Paperclips 10) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

treeCost :: Cost Integer
treeCost = Cost (Paperclips 0)
                (Helpers 0)
                (Trees 0)
                (TreeSeeds [GrowingDone])
                (Water 0)
                (Wood 0)

treeSeedCost :: Cost Integer
treeSeedCost = Cost (Paperclips 100)
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
noCost =
    Cost (Paperclips 0) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

getInitialState :: MyState
getInitialState =
    MyState config [] [] researchAreas resources (Seconds 0) (IsStarted False)
