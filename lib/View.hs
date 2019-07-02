module View where

import           Control.Lens                   ( view )
import           Data.Text                      ( Text )

import           Elements
import           Resources
import           Source
import           State

viewActions :: State a -> [Action a]
viewActions = view actions

viewEnergy :: State a -> Energy a
viewEnergy = view (resources . elements . elementEnergy . count)

viewErrorLog :: State a -> [ErrorLogLine]
viewErrorLog = view errorLog

viewPaperclips :: State a -> Paperclips a
viewPaperclips = view (resources . elements . elementPaperclips . count)

viewHelpers :: State a -> Helpers a
viewHelpers = view (resources . elements . elementHelpers . count)

viewStorage :: State a -> Storage (Paperclips a)
viewStorage = view (resources . storage)

viewTrees :: State a -> Trees a
viewTrees = view (resources . elements . elementTrees . count)

viewTreeSeeds :: State a -> TreeSeeds a
viewTreeSeeds = view (resources . elements . elementTreeSeeds . count)

viewWater :: State a -> Water a
viewWater = view (resources . elements . elementWater . count)

viewWaterTank :: State a -> WaterTank a
viewWaterTank = view (resources . waterTank)

viewWood :: State a -> Wood a
viewWood = view (resources . elements . elementWood . count)

viewAdvancedHelperResearch :: State a -> ResearchProgress a
viewAdvancedHelperResearch =
    view (researchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: State a -> Seconds a
viewSeconds = view seconds

viewSource :: State a -> Text
viewSource = view (source . sourceText)

viewSourceStatus :: State a -> Text
viewSourceStatus = view (source . sourceStatus)

viewIsStarted :: State a -> IsStarted
viewIsStarted = view isStarted
