module View where

import           Control.Lens                   ( view )

import           Elements
import           Mod
import           Resources

viewActions :: MyState a -> [Action a]
viewActions = view actions

viewErrorLog :: MyState a -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: MyState a -> Paperclips a
viewPaperclips = view (resources . elements . elementPaperclips . count)

viewHelpers :: MyState a -> Helpers a
viewHelpers = view (resources . elements . elementHelpers . count)

viewStorage :: MyState a -> Storage (Paperclips a)
viewStorage = view (resources . storage)

viewTrees :: MyState a -> Trees a
viewTrees = view (resources . elements . elementTrees . count)

viewTreeSeeds :: MyState a -> TreeSeeds a
viewTreeSeeds = view (resources . elements . elementTreeSeeds . count)

viewWater :: MyState a -> Water a
viewWater = view (resources . elements . elementWater . count)

viewWaterTank :: MyState a -> WaterTank a
viewWaterTank = view (resources . waterTank)

viewWood :: MyState a -> Wood a
viewWood = view (resources . elements . elementWood . count)

viewAdvancedHelperResearch :: MyState a -> ResearchProgress a
viewAdvancedHelperResearch =
    view (researchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: MyState a -> Seconds a
viewSeconds = view seconds

viewIsStarted :: MyState a -> IsStarted
viewIsStarted = view isStarted
