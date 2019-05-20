module Lib (module Lib, module Mod) where

import Control.Lens

import qualified BusinessLogic as BL
import LensUtils
import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork . researchWork

helperWork :: MyState -> MyState
helperWork state = setOutput state $ helperWork' $ getInput state
  where
    getInput = getInput4
      (resources.paperclips)
      (resources.helpers)
      (config.constants.helperInc)
      (resources.storage)
    setOutput = setOutput1 (resources.paperclips)

helperWork' :: (Paperclips, Helpers, HelperInc, Storage) -> Paperclips
helperWork' (p, h, inc, storage) = BL.helperWork p h inc storage

researchWork :: MyState -> MyState
researchWork state =
  case view (researchAreas.advancedHelperResearch.researchCompProgress) state of
    NotResearched -> state
    ResearchInProgress 1 -> set (researchAreas.advancedHelperResearch.researchCompProgress) ResearchDone $ over (config.constants.helperInc) (\(HelperInc inc) -> HelperInc (inc * 2)) state
    ResearchInProgress n -> set (researchAreas.advancedHelperResearch.researchCompProgress) (ResearchInProgress (n-1)) state
    ResearchDone -> state

addSecond :: MyState -> MyState
addSecond = over seconds succ

createPC :: MyState -> MyState
createPC = over (resources.paperclips) succ

buyHelper :: MyState -> MyState
buyHelper state = setOutput state $ buyHelper' $ getInput state
  where
    getInput = getInput5
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)
      errorLog
    setOutput = setOutput3 (resources.paperclips) (resources.helpers) errorLog

buyHelper' :: (Seconds, HelperPrice, Paperclips, Helpers, [ErrorLogLine]) -> (Paperclips, Helpers, [ErrorLogLine])
buyHelper' (s, hp, pc, helpers, errs) = case BL.buyHelper s hp pc helpers errs of
  Left errs' -> (pc, helpers, errs')
  Right (hp', pc') -> (pc', hp', errs)

researchAdvancedHelper :: MyState -> MyState
researchAdvancedHelper state = setOutput state $ researchAdvancedHelper' $ getInput state
  where
    getInput = getInput6
      seconds
      (resources.paperclips)
      (config.prices.advancedHelperPrice)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (researchAreas.advancedHelperResearch.researchCompDuration)
      errorLog
    setOutput = setOutput3
      (resources.paperclips)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      errorLog

researchAdvancedHelper' :: (Seconds, Paperclips, AdvancedHelperPrice, ResearchProgress, Duration, [ErrorLogLine]) -> (Paperclips, ResearchProgress, [ErrorLogLine])
researchAdvancedHelper' (s, pc, price, progress, duration, errs) = case BL.researchAdvancedHelper s pc price progress duration errs of
  Left errs' -> (pc, progress, errs')
  Right (pc', progress') -> (pc', progress', errs)

plantASeed :: MyState -> MyState
plantASeed state = setOutput state $ plantASeed' $ getInput state
  where
    getInput = getInput5
      seconds
      (config.prices.treePrice)
      (resources.treeSeeds)
      (resources.trees)
      errorLog
    setOutput = setOutput3 errorLog (resources.treeSeeds) (resources.trees)

plantASeed' :: (Seconds, TreePrice, TreeSeeds, Trees, [ErrorLogLine]) -> ([ErrorLogLine], TreeSeeds, Trees)
plantASeed' (s, price, seeds, trees, errs) = case BL.plantASeed s price seeds trees errs of
  Left errs' -> (errs', seeds, trees)
  Right (seeds', trees') -> (errs, seeds', trees')

setStarted :: MyState -> MyState
setStarted = over isStarted (const $ IsStarted True)

initialPrices :: Prices
initialPrices = Prices (AdvancedHelperPrice $ Paperclips 5) (HelperPrice $ Paperclips 10) (TreePrice $ TreeSeeds 1)

getInitialState :: MyState
getInitialState = MyState (Config (Constants (HelperInc 1)) initialPrices) [] [] (ResearchAreas (ResearchComp (Duration 10) NotResearched)) (Resources 0 0 (Storage 1000) 0 10) 0 (IsStarted False)