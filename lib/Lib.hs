module Lib (module Lib, module Mod, module Lenses) where

import Data.Bifoldable (Bifoldable, bifoldMap)
import Control.Lens

import Lenses
import Mod
import qualified PathedBusinessLogic as PBL

nextTick :: MyState -> MyState
nextTick = handleActions . addSecond . helperWork . seedWork . researchWork

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
applyAction (SetAdvancedHelperResearchProgress p) state = set (researchAreas.advancedHelperResearch.researchCompProgress) p state
applyAction (SetHelperInc i) state = set (config.constants.helperInc) i state
applyAction (SetProgs ps) state = set (resources.treeSeeds.progs) ps state

helperWork :: MyState -> MyState
helperWork state
  = addActions state
  $ (singleton . SetP)
  $ PBL.helperWork state

singleton :: a -> [a]
singleton = (:[])

researchWork :: MyState -> MyState
researchWork state
  = handleActions
  $ addActions state
  $ (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
  $ PBL.researchWork state

seedWork :: MyState -> MyState
seedWork state
  = addActions state
  $ (\(p,t) -> SetProgs p : SetTrees t : [])
  $ PBL.seedWork state

addSecond :: MyState -> MyState
addSecond = over seconds succ

createPaperclip :: MyState -> MyState
createPaperclip state
  = handleActions
  $ addActions state
  $ (\p -> SetP p : [])
  $ PBL.createPaperclip state

buyHelper :: MyState -> MyState
buyHelper state
  = handleActions
  $ addActions state
  $ withError (\(h,p) -> SetH h : SetP p : [])
  $ PBL.buyHelper state

withError :: Bifoldable p => (b -> [Action]) -> p ErrorLogLine b -> [Action]
withError = bifoldMap (singleton . SetE)

addActions :: MyState -> [Action] -> MyState
addActions state newActions = over actions (\as -> newActions ++ as) state

researchAdvancedHelper :: MyState -> MyState
researchAdvancedHelper state
  = handleActions
  $ addActions state
  $ withError (\(p,r) -> SetP p : SetR r : [])
  $ PBL.researchAdvancedHelper state

plantASeed :: MyState -> MyState
plantASeed state
  = handleActions
  $ addActions state
  $ withError (\s -> SetTreeSeeds s : [])
  $ PBL.plantASeed state

setStarted :: MyState -> MyState
setStarted = over isStarted (const $ IsStarted True)

initialPrices :: Prices
initialPrices = Prices (AdvancedHelperPrice $ Paperclips 5) (HelperPrice $ Paperclips 10) (TreePrice 1)

getInitialState :: MyState
getInitialState = MyState (Config (Constants (HelperInc (Helpers 1))) (Durations (TreeDuration 20)) initialPrices) [] [] (ResearchAreas (ResearchComp (Duration 10) NotResearched)) (Resources (Paperclips 0) (Helpers 0) (Storage 1000) (Trees 0) (TreeSeeds (replicate 20 NotGrowing)) (Wood 0)) (Seconds 0) (IsStarted False)
