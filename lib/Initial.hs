module Initial where

import Mod
import Resources

prices :: Prices
prices = Prices
    (AdvancedHelperPrice $ Paperclips 5)
    (HelperPrice $ Paperclips 10)
    (ProgPrice 2)
    (TreePrice 1)

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
    (Paperclips 0)
    (Helpers 0)
    (Storage 1000)
    (Trees 0)
    (TreeSeeds (replicate 10 NotGrowing))
    (Water 100)
    (WaterTank 100)
    (Wood 0)

getInitialState :: MyState
getInitialState = MyState config [] [] researchAreas resources (Seconds 0) (IsStarted False)