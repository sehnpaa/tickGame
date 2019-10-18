module Path where

import           Control.Lens

import           Elements
import           Resources
import           State

getPaperclipCount :: (HasState s a) => Getter s (Paperclips a)
getPaperclipCount =
    stateResources . resourcesElements . elementsPaperclips . count

getStorage :: (HasState s a) => Getter s (Paperclips a)
getStorage = stateResources . resourcesStorage . unStorage
