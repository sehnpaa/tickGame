module View where

import Control.Lens

import Lenses
import Mod

viewActions :: MyState -> [Action]
viewActions = view actions

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: MyState -> Paperclips
viewPaperclips = view (resources.paperclips)

viewHelpers :: MyState -> Helpers
viewHelpers = view (resources.helpers)

viewStorage :: MyState -> Storage
viewStorage = view (resources.storage)

viewTrees :: MyState -> Trees
viewTrees = view (resources.trees)

viewTreeSeeds :: MyState -> TreeSeeds
viewTreeSeeds = view (resources.treeSeeds)

viewAdvancedHelperResearch :: MyState -> ResearchProgress
viewAdvancedHelperResearch = view (researchAreas.advancedHelperResearch.researchCompProgress)

viewSeconds :: MyState -> Seconds
viewSeconds = view seconds

viewWood :: MyState -> Wood
viewWood = view (resources.wood)

viewIsStarted :: MyState -> IsStarted
viewIsStarted = view isStarted
