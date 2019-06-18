module View where

import Control.Lens

import Lenses
import Mod
import Resources

viewActions :: MyState -> [Action]
viewActions = view actions

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: MyState -> Paperclips Integer
viewPaperclips = view (resources.paperclips)

viewHelpers :: MyState -> Helpers Integer
viewHelpers = view (resources.helpers)

viewStorage :: MyState -> Storage (Paperclips Integer)
viewStorage = view (resources.storage)

viewTrees :: MyState -> Trees
viewTrees = view (resources.trees)

viewTreeSeeds :: MyState -> TreeSeeds
viewTreeSeeds = view (resources.treeSeeds)

viewWater :: MyState -> Water Integer
viewWater = view (resources.water)

viewWaterTank :: MyState -> WaterTank Integer
viewWaterTank = view (resources.waterTank)

viewWood :: MyState -> Wood
viewWood = view (resources.wood)

viewAdvancedHelperResearch :: MyState -> ResearchProgress
viewAdvancedHelperResearch = view (researchAreas.advancedHelperResearch.researchCompProgress)

viewSeconds :: MyState -> Seconds
viewSeconds = view seconds

viewIsStarted :: MyState -> IsStarted
viewIsStarted = view isStarted
