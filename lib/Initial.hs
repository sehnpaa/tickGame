module Initial where

import Config
import Elements
import Mod
import Resources

prices :: Prices
prices = Prices
    (AdvancedHelperPrice $ Paperclips 5)
    (HelperPrice $ Paperclips 10)
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
    (Paperclips 0)
    (Helpers 0)
    (Trees 0)
    (TreeSeeds (replicate 10 NotGrowing))
    (Water 100)
    (Wood 0)

getInitialState :: MyState
getInitialState = MyState config [] [] researchAreas resources (Seconds 0) (IsStarted False)