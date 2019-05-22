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
applyAction (SetR r) state = set (researchAreas.advancedHelperResearch.researchCompProgress) r state
applyAction (SetTreeSeeds s) state = set (resources.treeSeeds) s state
applyAction (SetTrees t) state = set (resources.trees) t state

helperWork :: MyState -> MyState
helperWork state
  = addActions state
  $ (singleton . SetP)
  $ arg4 BL.helperWork
      (resources.paperclips)
      (resources.helpers)
      (config.constants.helperInc)
      (resources.storage)
      state

singleton :: a -> [a]
singleton = (:[])

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
buyHelper state
  = handleActions
  $ addActions state
  $ bifoldMap (singleton . SetE) (\(h,p) -> SetH h : SetP p : [])
  $ arg4 BL.buyHelper
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)
      state

addActions :: MyState -> [Action] -> MyState
addActions state newActions = over actions (\as -> newActions ++ as) state

researchAdvancedHelper :: MyState -> MyState
researchAdvancedHelper state
  = handleActions
  $ addActions state
  $ bifoldMap (singleton . SetE) (\(p,r) -> SetP p : SetR r : [])
  $ arg5 BL.researchAdvancedHelper
      seconds
      (resources.paperclips)
      (config.prices.advancedHelperPrice)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (researchAreas.advancedHelperResearch.researchCompDuration)
      state

plantASeed :: MyState -> MyState
plantASeed state
  = handleActions
  $ addActions state
  $ bifoldMap (singleton . SetE) (\(s,t) -> SetTreeSeeds s : SetTrees t : [])
  $ arg4 BL.plantASeed seconds (config.prices.treePrice) (resources.treeSeeds) (resources.trees) state

setStarted :: MyState -> MyState
setStarted = over isStarted (const $ IsStarted True)

initialPrices :: Prices
initialPrices = Prices (AdvancedHelperPrice $ Paperclips 5) (HelperPrice $ Paperclips 10) (TreePrice $ TreeSeeds 1)

getInitialState :: MyState
getInitialState = MyState (Config (Constants (HelperInc 1)) initialPrices) [] [] (ResearchAreas (ResearchComp (Duration 10) NotResearched)) (Resources 0 0 (Storage 1000) 0 10) 0 (IsStarted False)