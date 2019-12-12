module View where

import           Control.Lens                   ( view )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                , runReader
                                                )

import           Elements
import           Resources
import           Seconds
import           Source
import           State

viewActions :: (HasState s a) => s -> [Action a]
viewActions = view (stateActions . unActions)

viewEnergy :: (HasState s a) => s -> Energy a
viewEnergy = view (stateResources . resourcesElements . elementsEnergy . count)

viewErrorLog :: (HasState s a) => s -> [ErrorLogLine]
viewErrorLog = view stateErrorLog

viewPaperclips :: (Show a, HasState s a) => s -> Paperclips a
viewPaperclips =
    view (stateResources . resourcesElements . elementsPaperclips . count)

viewHelpers :: (HasState s a) => s -> Helpers a
viewHelpers =
    view (stateResources . resourcesElements . elementsHelpers . count)

viewStorageOfPaperclips :: (HasState s a) => s -> StorageOfPaperclips a
viewStorageOfPaperclips = view (stateResources . resourcesStorage)

viewTrees :: (HasState s a) => s -> Trees a
viewTrees = view (stateResources . resourcesElements . elementsTrees . count)

viewTreeSeeds :: (HasState s a) => s -> TreeSeeds a
viewTreeSeeds =
    view (stateResources . resourcesElements . elementsTreeSeeds . count)

viewWater :: (HasState s a) => s -> Water a
viewWater = view (stateResources . resourcesElements . elementsWater . count)

viewWaterTank :: (HasState s a) => s -> WaterTank a
viewWaterTank = view (stateResources . resourcesWaterTank)

viewWood :: (HasState s a) => s -> Wood a
viewWood = view (stateResources . resourcesElements . elementsWood . count)

viewAdvancedHelperResearch :: (HasState s a) => s -> ResearchProgress a
viewAdvancedHelperResearch =
    view (stateResearchAreas . advancedHelperResearch . researchCompProgress)

viewSeconds :: (HasState s a) => s -> Seconds a
viewSeconds = view stateSeconds

viewSnapshots :: (HasState s a) => s -> Snapshots a
viewSnapshots = view stateSnapshots

viewSource :: (HasState s a) => s -> SourceText
viewSource = view (stateSource . sourceText)

viewSourceStatus :: (HasState s a) => s -> SourceStatus
viewSourceStatus = view (stateSource . sourceStatus)

viewIsStarted :: (HasState s a) => s -> IsStarted
viewIsStarted = view stateIsStarted

viewCreatePaperclip :: (HasState s a) => s -> EventCreatePaperclip
viewCreatePaperclip = view (stateEvents . eventsEventCreatePaperclip)

viewButtonData2
    :: ( HasAcquirePaperclips s a
       , HasButtonDisplayStatus s
       , HasEventCreatePaperclip s
       , HasEventStart s
       , HasState s a
       , Show a
       )
    => Button
    -> s
    -> ButtonDataAPI
viewButtonData2 ButtonStart           = runReader buttonStartButton
viewButtonData2 ButtonCreatePaperclip = runReader createPaperclipButton

createPaperclipButton
    :: (HasAcquirePaperclips s a, HasEventCreatePaperclip s, MonadReader s m)
    => m ButtonDataAPI
createPaperclipButton = do
    (EventCreatePaperclip a    ) <- ask $ view eventCreatePaperclip
    (PaperclipsManually   cost') <- ask $ view paperclipsManually
    let b = case getStatus a of
            Enabled -> case getTooltipForEnabled a of
                ShowNothing  -> show ShowNothing
                ShowStatic s -> show $ ShowStatic s
                ShowCost     -> show cost'
            Disabled -> case getTooltipForDisabled a of
                ShowNothing  -> show ShowNothing
                ShowStatic s -> show $ ShowStatic s
                ShowCost     -> show cost'
            Hidden -> show Hidden
    return $ ButtonDataAPI a b

getStatus2 :: (HasButtonData s) => s -> Status
getStatus2 = view (buttonDataStatus . status)

ff :: (HasButtonData s, HasButtonDisplayStatus s, MonadReader s m) => m String
ff = do
    a            <- ask $ view id
    whenEnabled  <- ask $ view displayStatusEnabled
    whenDisabled <- ask $ view displayStatusDisabled
    return $ case getStatus2 a of
        Enabled -> case whenEnabled of
            ShowNothing  -> show ShowNothing
            ShowStatic s -> show $ ShowStatic s
            ShowCost     -> show $ ShowCost
        Disabled -> case whenDisabled of
            ShowNothing  -> show ShowNothing
            ShowStatic s -> show $ ShowStatic s
            ShowCost     -> show $ ShowCost
        Hidden -> show Hidden

buttonStartButton
    :: ( HasButtonDisplayStatus s
       , HasEventCreatePaperclip s
       , HasEventStart s
       , MonadReader s m
       )
    => m ButtonDataAPI
buttonStartButton = do
    a <- ask $ view eventStartButtonData
    return $ ButtonDataAPI a (ff a)

getStatus :: ButtonData -> Status
getStatus = view (buttonDataStatus . status)

getTooltipForEnabled :: ButtonData -> Tooltip
getTooltipForEnabled = view (buttonDataDisplayStatus . displayStatusEnabled)

getTooltipForDisabled :: ButtonData -> Tooltip
getTooltipForDisabled = view (buttonDataDisplayStatus . displayStatusDisabled)

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

viewTitle :: (HasState s a) => s -> Title
viewTitle = view stateTitle
