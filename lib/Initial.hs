module Initial where

import Mod

prices :: Prices
prices = Prices
    (AdvancedHelperPrice $ Paperclips 5)
    (HelperPrice $ Paperclips 10)
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
    (TreeSeeds (replicate 20 NotGrowing))
    (Water 0)
    (Wood 0)

getInitialState :: MyState
getInitialState = MyState config [] [] researchAreas resources (Seconds 0) (IsStarted False)