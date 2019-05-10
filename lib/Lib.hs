{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Control.Lens
import Data.Text (concat, pack)

import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork

helperWork :: MyState -> MyState
helperWork state =
  let h = view (resources.helpers) state
  in over (resources.paperclips) (addHelperWork h) state

addHelperWork :: Helpers -> Paperclips -> Paperclips
addHelperWork h p = Paperclips $ (unPaperclips p) + (unHelpers h) * 2

addSecond :: MyState -> MyState
addSecond = over seconds succ

createPC :: MyState -> MyState
createPC = over (resources.paperclips) succ

buyHelper :: MyState -> MyState
buyHelper state =
  let paperclips' = view (resources.paperclips) state
      price = view (config.prices.helperPrices) state
      s' = view seconds state
    in if (unHelperPrice price) > paperclips'
      then over errorLog (addToErrorLog (lineNeedMorePaperclips s')) state
      else over (resources.helpers) succ $ over (resources.paperclips) (decPaperclipsWith price) state

decPaperclipsWith :: HelperPrice -> Paperclips -> Paperclips
decPaperclipsWith price paperclips = paperclips - (unHelperPrice price)

addToErrorLog :: ErrorLogLine -> [ErrorLogLine] -> [ErrorLogLine]
addToErrorLog new existing = existing ++ [new]

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

plantASeed :: MyState -> MyState
plantASeed = over (resources.treeSeeds) pred

getInitialState :: MyState
getInitialState = MyState (Config (Prices (HelperPrice $ Paperclips 10))) [] [] (Resources 0 0 0 10) 0 (IsStarted False)