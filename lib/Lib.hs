module Lib (module Lib, module Mod) where

import Data.Bifoldable (bifoldMap)
import Control.Lens

import qualified BusinessLogic as BL
import LensUtils
import Mod

nextTick :: MyState -> MyState
nextTick = handleActions . addSecond . helperWork . researchWork

handleActions :: MyState -> MyState
handleActions state = let as = view actions state
  in
    set actions [] $ foldr applyAction state as

applyAction :: Action -> MyState -> MyState
applyAction (SetP p) state = set (resources.paperclips) p state
applyAction (SetH h) state = set (resources.helpers) h state
applyAction (SetE err) state = over errorLog (\errs -> err : errs) state

helperWork :: MyState -> MyState
helperWork state = addActions state $ (:[]) $ SetP $ uncurry4 BL.helperWork $ getInput state
  where
    getInput = getInput4
      (resources.paperclips)
      (resources.helpers)
      (config.constants.helperInc)
      (resources.storage)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a,b,c,d) = f a b c d

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
buyHelper state = handleActions $ addActions state $ bifoldMap ((:[]) . SetE) (\(h,p) -> SetH h : SetP p : []) $ uncurry4 BL.buyHelper $ getInput state
  where
    getInput = getInput4
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)

addActions :: MyState -> [Action] -> MyState
addActions state newActions = over actions (\as -> newActions ++ as) state

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