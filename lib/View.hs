module View where

import           Control.Lens                   ( view )

import           Elements
import           Resources
import           Seconds
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

viewSnapshots :: State a -> Snapshots a
viewSnapshots = view snapshots

viewSource :: State a -> SourceText
viewSource = view (source . sourceText)

viewSourceStatus :: State a -> SourceStatus
viewSourceStatus = view (source . sourceStatus)

viewIsStarted :: State a -> IsStarted
viewIsStarted = view isStarted

viewCreatePaperclip :: State a -> EventCreatePaperclip
viewCreatePaperclip = view (events . eventCreatePaperclip)

viewButtonData :: Button -> State a -> ButtonData
viewButtonData ButtonStart = view (events . eventStart . eventStartButtonData)
viewButtonData ButtonCreatePaperclip =
    view (events . eventCreatePaperclip . eventCreatePaperclipButtonData)
viewButtonData ButtonCreateHelper =
    view (events . eventCreateHelper . eventCreateHelperButtonData)
viewButtonData ButtonExtendStorage =
    view (events . eventExtendStorage . eventExtendStorageButtonData)
viewButtonData ButtonPumpWater =
    view (events . eventPumpWater . eventPumpWaterButtonData)
viewButtonData ButtonGenerateEnergy =
    view (events . eventGenerateEnergy . eventGenerateEnergyButtonData)
viewButtonData ButtonBuyASeed =
    view (events . eventBuyASeed . eventBuyASeedButtonData)
viewButtonData ButtonPlantASeed =
    view (events . eventPlantASeed . eventPlantASeedButtonData)
viewButtonData ButtonResearchAdvancedHelper = view
    ( events
    . eventResearchAdvancedHelper
    . eventResearchAdvancedHelperButtonData
    )
viewButtonData ButtonPreviousSnapshot =
    view (events . eventPreviousSnapshot . eventPreviousSnapshotButtonData)
viewButtonData ButtonNextSnapshot =
    view (events . eventNextSnapshot . eventNextSnapshotButtonData)
viewButtonData ButtonApplySnapshot =
    view (events . eventApplySnapshot . eventApplySnapshotButtonData)
viewButtonData ButtonExitApplication =
    view (events . eventExitApplication . eventExitApplicationButtonData)

viewTitle :: State a -> Title
viewTitle = view title
