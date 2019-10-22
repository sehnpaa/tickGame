module View where

import           Control.Lens                   ( view )

import           Elements
import           Resources
import           Seconds
import           Source
import           State

viewActions :: State a -> [Action a]
viewActions = view stateActions

viewEnergy :: State a -> Energy a
viewEnergy = view (stateResources . resourcesElements . elementsEnergy . count)

viewErrorLog :: State a -> [ErrorLogLine]
viewErrorLog = view stateErrorLog

viewPaperclips :: State a -> Paperclips a
viewPaperclips =
    view (stateResources . resourcesElements . elementsPaperclips . count)

viewHelpers :: State a -> Helpers a
viewHelpers =
    view (stateResources . resourcesElements . elementsHelpers . count)

viewStorage :: State a -> StorageOfPaperclips a
viewStorage = view (stateResources . resourcesStorage)

viewTrees :: State a -> Trees a
viewTrees = view (stateResources . resourcesElements . elementsTrees . count)

viewTreeSeeds :: State a -> TreeSeeds a
viewTreeSeeds =
    view (stateResources . resourcesElements . elementsTreeSeeds . count)

viewWater :: State a -> Water a
viewWater = view (stateResources . resourcesElements . elementsWater . count)

viewWaterTank :: State a -> WaterTank a
viewWaterTank = view (stateResources . resourcesWaterTank)

viewWood :: State a -> Wood a
viewWood = view (stateResources . resourcesElements . elementsWood . count)

viewAdvancedHelperResearch :: State a -> ResearchProgress a
viewAdvancedHelperResearch =
    view (stateResearchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: State a -> Seconds a
viewSeconds = view stateSeconds

viewSnapshots :: State a -> Snapshots a
viewSnapshots = view stateSnapshots

viewSource :: State a -> SourceText
viewSource = view (stateSource . sourceText)

viewSourceStatus :: State a -> SourceStatus
viewSourceStatus = view (stateSource . sourceStatus)

viewIsStarted :: State a -> IsStarted
viewIsStarted = view stateIsStarted

viewCreatePaperclip :: State a -> EventCreatePaperclip
viewCreatePaperclip = view (stateEvents . eventsEventCreatePaperclip)

viewButtonData :: Button -> State a -> ButtonData
viewButtonData ButtonStart =
    view (stateEvents . eventsEventStart . eventStartButtonData)
viewButtonData ButtonCreatePaperclip = view
    (stateEvents . eventsEventCreatePaperclip . eventCreatePaperclipButtonData)
viewButtonData ButtonCreateHelper =
    view (stateEvents . eventsEventCreateHelper . eventCreateHelperButtonData)
viewButtonData ButtonExtendStorage =
    view (stateEvents . eventsEventExtendStorage . eventExtendStorageButtonData)
viewButtonData ButtonPumpWater =
    view (stateEvents . eventsEventPumpWater . eventPumpWaterButtonData)
viewButtonData ButtonGenerateEnergy = view
    (stateEvents . eventsEventGenerateEnergy . eventGenerateEnergyButtonData)
viewButtonData ButtonBuyASeed =
    view (stateEvents . eventsEventBuyASeed . eventBuyASeedButtonData)
viewButtonData ButtonPlantASeed =
    view (stateEvents . eventsEventPlantASeed . eventPlantASeedButtonData)
viewButtonData ButtonResearchAdvancedHelper = view
    ( stateEvents
    . eventsEventResearchAdvancedHelper
    . eventResearchAdvancedHelperButtonData
    )
viewButtonData ButtonPreviousSnapshot = view
    (stateEvents . eventsEventPreviousSnapshot . eventPreviousSnapshotButtonData
    )
viewButtonData ButtonNextSnapshot =
    view (stateEvents . eventsEventNextSnapshot . eventNextSnapshotButtonData)
viewButtonData ButtonApplySnapshot =
    view (stateEvents . eventsEventApplySnapshot . eventApplySnapshotButtonData)
viewButtonData ButtonExitApplication = view
    (stateEvents . eventsEventExitApplication . eventExitApplicationButtonData)

viewTitle :: State a -> Title
viewTitle = view stateTitle
