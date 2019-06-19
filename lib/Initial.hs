module Initial where

import Config
import Elements
import Mod
import Resources

prices :: Prices
prices = Prices
    (AdvancedHelperPrice $ Paperclips 5)
    (ProgPrice 2)
    (TreePrice 1)
    (TreeSeedPrice $ Paperclips 100)

config :: Config
config = Config
    (Constants (HelperInc (Helpers 1)))
    (Durations (TreeDuration 20))
    prices

researchAreas :: ResearchAreas
researchAreas = ResearchAreas
    (ResearchComp (Duration 10) NotResearched)

resources :: Resources
resources = Resources
    elements
    (Storage (Paperclips 1000))
    (WaterTank 100)

elements :: Elements
elements = Elements
    (Element (AcquirePaperclips Initial.paperclipCost) $ Paperclips 0)
    (Element (AcquireHelpers helperCost) $ Helpers 0)
    (Element (AcquireTrees treeCost) $ Trees 0)
    (Element (AcquireTreeSeeds treeSeedCost) $ TreeSeeds (replicate 10 NotGrowing))
    (Element AcquireWater $ Water 100)
    (Element AcquireWood $ Wood 0)

paperclipCost :: Cost Integer
paperclipCost = Cost (Paperclips 10) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

helperCost :: Cost Integer
helperCost = noCost

treeCost :: Cost Integer
treeCost = noCost

treeSeedCost :: Cost Integer
treeSeedCost = noCost

noCost :: Cost Integer
noCost = Cost (Paperclips 0) (Helpers 0) (Trees 0) (TreeSeeds []) (Water 0) (Wood 0)

getInitialState :: MyState
getInitialState = MyState config [] [] researchAreas resources (Seconds 0) (IsStarted False)