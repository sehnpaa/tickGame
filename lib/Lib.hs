{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Data.Text (concat, pack)
import Lens.Micro.Platform

import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork

helperWork :: MyState -> MyState
helperWork state =
  let h = view helpers state
  in over paperclips (addHelperWork h) state

addHelperWork :: Helpers -> Paperclips -> Paperclips
addHelperWork h p = Paperclips $ (unPaperclips p) + (unHelpers h) * 2

addSecond :: MyState -> MyState
addSecond = over seconds succ

createPC :: MyState -> MyState
createPC = over paperclips succ

buyHelper :: MyState -> MyState
buyHelper state@(MyState conf as es p h t s (IsStarted True)) =
  let price = view (config.prices.helperPrices) state
      s' = view seconds state
    in if p < price
      then over errorLog (addToErrorLog (lineNeedMorePaperclips s')) state
      else MyState conf as es (p - price) (succ h) t s (IsStarted True)

addToErrorLog :: ErrorLogLine -> [ErrorLogLine] -> [ErrorLogLine]
addToErrorLog new existing = existing ++ [new]

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]


plantASeed :: MyState -> MyState
plantASeed (MyState conf as es p h t s (IsStarted True)) = MyState conf as es p h (pred t) s (IsStarted True)

getInitialState :: MyState
getInitialState = MyState (Config (Prices (Paperclips 10))) [] [] 0 0 10 0 (IsStarted False)