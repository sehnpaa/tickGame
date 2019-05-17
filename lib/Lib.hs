module Lib (module Lib, module Mod) where

import Control.Lens

import qualified BusinessLogic as BL
import LensUtils
import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork . researchWork

helperWork :: MyState -> MyState
helperWork state =
  let h = view (resources.helpers) state
  in over (resources.paperclips) (addHelperWork h) state

researchWork :: MyState -> MyState
researchWork state =
  case view (researchAreas.advancedHelperResearch.researchCompProgress) state of
    NotResearched -> state
    ResearchInProgress 1 -> set (researchAreas.advancedHelperResearch.researchCompProgress) ResearchDone state
    ResearchInProgress n -> set (researchAreas.advancedHelperResearch.researchCompProgress) (ResearchInProgress (n-1)) state
    ResearchDone -> state

addHelperWork :: Helpers -> Paperclips -> Paperclips
addHelperWork h p = Paperclips $ (unPaperclips p) + (unHelpers h) * 2

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
buyHelper' (s, hp, pc, helpers, errs )= case BL.buyHelper s hp pc helpers errs of
  Left errs' -> (pc, helpers, errs')
  Right (hp', pc') -> (pc', hp', errs)

researchAdvancedHelper :: MyState -> MyState
researchAdvancedHelper state = setOutput state $ researchAdvancedHelper' $ getInput state
  where
    getInput = getInput5
      seconds
      (resources.paperclips)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (researchAreas.advancedHelperResearch.researchCompDuration)
      errorLog
    setOutput = setOutput3
      (resources.paperclips)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      errorLog

researchAdvancedHelper' :: (Seconds, Paperclips, ResearchProgress, Duration, [ErrorLogLine]) -> (Paperclips, ResearchProgress, [ErrorLogLine])
researchAdvancedHelper' (s, pc, progress, duration, errs) = case BL.researchAdvancedHelper s pc progress duration errs of
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
initialPrices = Prices (HelperPrice $ Paperclips 10) (TreePrice $ TreeSeeds 1)

getInitialState :: MyState
getInitialState = MyState (Config initialPrices) [] [] (ResearchAreas (ResearchComp (Duration 30) NotResearched)) (Resources 0 0 0 10) 0 (IsStarted False)