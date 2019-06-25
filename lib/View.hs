module View where

import           Control.Lens                   ( view )

import           Elements
import           Mod
import           Resources

viewActions :: MyState -> [Action]
viewActions = view actions

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: MyState -> Paperclips Integer
viewPaperclips = view (resources . elements . elementPaperclips . count)

viewHelpers :: MyState -> Helpers Integer
viewHelpers = view (resources . elements . elementHelpers . count)

viewStorage :: MyState -> Storage (Paperclips Integer)
viewStorage = view (resources . storage)

viewTrees :: MyState -> Trees Integer
viewTrees = view (resources . elements . elementTrees . count)

viewTreeSeeds :: MyState -> TreeSeeds Integer
viewTreeSeeds = view (resources . elements . elementTreeSeeds . count)

viewWater :: MyState -> Water Integer
viewWater = view (resources . elements . elementWater . count)

viewWaterTank :: MyState -> WaterTank Integer
viewWaterTank = view (resources . waterTank)

viewWood :: MyState -> Wood Integer
viewWood = view (resources . elements . elementWood . count)

viewAdvancedHelperResearch :: MyState -> ResearchProgress
viewAdvancedHelperResearch =
    view (researchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: MyState -> Seconds
viewSeconds = view seconds

viewIsStarted :: MyState -> IsStarted
viewIsStarted = view isStarted
