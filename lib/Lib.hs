{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Control.Lens
import Data.Text (concat, pack)

import qualified BusinessLogic as BL
import LensUtils
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
  setOutput state $ buyHelper' $ getInput state
  where
    getInput = getInput5
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)
      errorLog
    setOutput = setOutput3 (resources.paperclips) (resources.helpers) errorLog

buyHelper' :: (Seconds, HelperPrice, Paperclips, Helpers, [ErrorLogLine]) -> (Paperclips, Helpers, [ErrorLogLine])
buyHelper' (s, hp, pc, helpers, errs )= case BL.buyHelper s hp pc helpers errs of
  Left errs' -> (pc, helpers, errs')
  Right (hp', pc') -> (pc', hp', errs)

plantASeed :: MyState -> MyState
plantASeed state =
  let seeds = view (resources.treeSeeds) state
      s' = view seconds state
    in
      if 1 > (unTreeSeeds seeds)
        then over errorLog (BL.addToErrorLog (lineNeedMoreSeeds s')) state
        else over (resources.trees) succ $ over (resources.treeSeeds) pred state

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

setStarted :: MyState -> MyState
setStarted = over isStarted (const $ IsStarted True)

initialPrices :: Prices
initialPrices = Prices (HelperPrice $ Paperclips 10) (TreePrice $ TreeSeeds 1)

getInitialState :: MyState
getInitialState = MyState (Config initialPrices) [] [] (Resources 0 0 0 10) 0 (IsStarted False)