module View where

import           Control.Lens                   ( view )

import           Elements
import           Resources
import           Seconds
import           Source
import           State

viewActions :: State a -> [Action a]
viewActions = view (stateActions . unActions)

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

viewButtonData :: (Show a) => Button -> State a -> ButtonDataAPI
viewButtonData ButtonStart st = ButtonDataAPI
    (view (stateEvents . eventsEventStart . eventStartButtonData) st)
    (case
          (view
              ( stateEvents
              . eventsEventStart
              . eventStartButtonData
              . buttonDataStatus
              . status
              )
              st
          )
      of
          Enabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventStart
                          . eventStartButtonData
                          . buttonDataDisplayStatus
                          . displayStatusEnabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost     -> show $ ShowCost
          Disabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventStart
                          . eventStartButtonData
                          . buttonDataDisplayStatus
                          . displayStatusDisabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost     -> show $ ShowCost
          Hidden -> show Hidden
    )
viewButtonData ButtonCreatePaperclip st = ButtonDataAPI
    (view
        ( stateEvents
        . eventsEventCreatePaperclip
        . eventCreatePaperclipButtonData
        )
        st
    )
    (case
          (view
              ( stateEvents
              . eventsEventCreatePaperclip
              . eventCreatePaperclipButtonData
              . buttonDataStatus
              . status
              )
              st
          )
      of
          Enabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventCreatePaperclip
                          . eventCreatePaperclipButtonData
                          . buttonDataDisplayStatus
                          . displayStatusEnabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost ->
                          (show $ unPaperclipsManually $ view
                              ( stateResources
                              . resourcesElements
                              . elementsPaperclips
                              . elementCost
                              . acquirePaperclips
                              . paperclipsManually
                              )
                              st
                          )
          Disabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventCreatePaperclip
                          . eventCreatePaperclipButtonData
                          . buttonDataDisplayStatus
                          . displayStatusDisabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost ->
                          (show $ unPaperclipsManually $ view
                              ( stateResources
                              . resourcesElements
                              . elementsPaperclips
                              . elementCost
                              . acquirePaperclips
                              . paperclipsManually
                              )
                              st
                          )
          Hidden -> show Hidden
    )
viewButtonData ButtonCreateHelper st = ButtonDataAPI
    (view
        (stateEvents . eventsEventCreateHelper . eventCreateHelperButtonData)
        st
    )
    (case
          (view
              ( stateEvents
              . eventsEventCreateHelper
              . eventCreateHelperButtonData
              . buttonDataStatus
              . status
              )
              st
          )
      of
          Enabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventCreateHelper
                          . eventCreateHelperButtonData
                          . buttonDataDisplayStatus
                          . displayStatusEnabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost ->
                          (show $ view
                              ( stateResources
                              . resourcesElements
                              . elementsHelpers
                              . elementCost
                              . acquireHelpersManually
                              . helpersManuallyCost
                              )
                              st
                          )
          Disabled ->
              case
                      (view
                          ( stateEvents
                          . eventsEventCreateHelper
                          . eventCreateHelperButtonData
                          . buttonDataDisplayStatus
                          . displayStatusDisabled
                          )
                          st
                      )
                  of
                      ShowNothing  -> show ShowNothing
                      ShowStatic s -> show $ ShowStatic s
                      ShowCost ->
                          (show $ view
                              ( stateResources
                              . resourcesElements
                              . elementsHelpers
                              . elementCost
                              . acquireHelpersManually
                              . helpersManuallyCost
                              )
                              st
                          )
          Hidden -> show Hidden
    )
viewButtonData ButtonExtendStorage st = ButtonDataAPI
    (view
        (stateEvents . eventsEventExtendStorage . eventExtendStorageButtonData)
        st
    )
    ""
viewButtonData ButtonPumpWater st = ButtonDataAPI
    (view (stateEvents . eventsEventPumpWater . eventPumpWaterButtonData) st)
    ""
viewButtonData ButtonGenerateEnergy st = ButtonDataAPI
    (view
        (stateEvents . eventsEventGenerateEnergy . eventGenerateEnergyButtonData
        )
        st
    )
    ""
viewButtonData ButtonBuyASeed st = ButtonDataAPI
    (view (stateEvents . eventsEventBuyASeed . eventBuyASeedButtonData) st)
    ""
viewButtonData ButtonPlantASeed st = ButtonDataAPI
    (view (stateEvents . eventsEventPlantASeed . eventPlantASeedButtonData) st)
    ""
viewButtonData ButtonResearchAdvancedHelper st = ButtonDataAPI
    (view
        ( stateEvents
        . eventsEventResearchAdvancedHelper
        . eventResearchAdvancedHelperButtonData
        )
        st
    )
    ""
viewButtonData ButtonPreviousSnapshot st = ButtonDataAPI
    (view
        ( stateEvents
        . eventsEventPreviousSnapshot
        . eventPreviousSnapshotButtonData
        )
        st
    )
    ""
viewButtonData ButtonNextSnapshot st = ButtonDataAPI
    (view
        (stateEvents . eventsEventNextSnapshot . eventNextSnapshotButtonData)
        st
    )
    ""
viewButtonData ButtonApplySnapshot st = ButtonDataAPI
    (view
        (stateEvents . eventsEventApplySnapshot . eventApplySnapshotButtonData)
        st
    )
    ""
viewButtonData ButtonExitApplication st = ButtonDataAPI
    (view
        ( stateEvents
        . eventsEventExitApplication
        . eventExitApplicationButtonData
        )
        st
    )
    ""

viewTitle :: State a -> Title
viewTitle = view stateTitle
