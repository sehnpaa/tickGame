module View where

import           Control.Lens                   ( view )

import           Elements
import           Lenses
import           Mod
import           Resources

viewActions :: MyState -> [Action]
viewActions = view actions

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: MyState -> Paperclips Integer
viewPaperclips = view (resources . elements . paperclips)

viewHelpers :: MyState -> Helpers Integer
viewHelpers = view (resources . elements . helpers)

viewStorage :: MyState -> Storage (Paperclips Integer)
viewStorage = view (resources . storage)

viewTrees :: MyState -> Trees Integer
viewTrees = view (resources . elements . trees)

viewTreeSeeds :: MyState -> TreeSeeds Integer
viewTreeSeeds = view (resources . elements . treeSeeds)

viewWater :: MyState -> Water Integer
viewWater = view (resources . elements . water)

viewWaterTank :: MyState -> WaterTank Integer
viewWaterTank = view (resources . waterTank)

viewWood :: MyState -> Wood Integer
viewWood = view (resources . elements . wood)

viewAdvancedHelperResearch :: MyState -> ResearchProgress
viewAdvancedHelperResearch =
    view (researchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: MyState -> Seconds
viewSeconds = view seconds

viewIsStarted :: MyState -> IsStarted
viewIsStarted = view isStarted
