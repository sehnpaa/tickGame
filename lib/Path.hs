module Path where

import           Control.Lens
import qualified Data.Text                     as T

import           Elements
import           Resources
import           Seconds
import           State

getSeconds :: (HasState s a) => Getter s (Seconds a)
getSeconds = stateSeconds

getPaperclipCount :: (HasState s a) => Getter s (Paperclips a)
getPaperclipCount =
    stateResources . resourcesElements . elementsPaperclips . count

getEnergyCount :: (HasState s a) => Getter s (Energy a)
getEnergyCount = (stateResources . resourcesElements . elementsEnergy . count)

getHelperCount :: (HasState s a) => Getter s (Helpers a)
getHelperCount = stateResources . resourcesElements . elementsHelpers . count

getHelpersManuallyCostEnergy :: (HasState s a) => Getter s (Energy a)
getHelpersManuallyCostEnergy =
    ( stateResources
    . resourcesElements
    . elementsHelpers
    . elementCost
    . acquireHelpersManually
    . helpersManuallyCost
    . costEnergyPaperclipsE
    )

getHelpersManuallyCostPaperclips :: (HasState s a) => Getter s (Paperclips a)
getHelpersManuallyCostPaperclips =
    ( stateResources
    . resourcesElements
    . elementsHelpers
    . elementCost
    . acquireHelpersManually
    . helpersManuallyCost
    . costEnergyPaperclipsP
    )

getHelpersManuallyEnergyErrorMessage :: (HasState s a) => Getter s T.Text
getHelpersManuallyEnergyErrorMessage =
    ( stateResources
    . resourcesElements
    . elementsHelpers
    . elementCost
    . acquireHelpersManually
    . helpersManuallyPaperclipsErrorMessage
    )

getHelpersManuallyPaperclipsErrorMessage :: (HasState s a) => Getter s T.Text
getHelpersManuallyPaperclipsErrorMessage =
    ( stateResources
    . resourcesElements
    . elementsHelpers
    . elementCost
    . acquireHelpersManually
    . helpersManuallyPaperclipsErrorMessage
    )

