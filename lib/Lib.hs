module Lib
  ( Initial.getInitialState
  , module Lenses
  , module Lib
  , module Mod
  , module Resources ) where

import Data.Bifoldable (Bifoldable, bifoldMap)
import Control.Lens

import qualified Initial as Initial
import Lenses
import Mod
import qualified PathedBusinessLogic as PBL
import Resources

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
applyAction (SetWater w) state = set (resources.water) w state

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
  $ withExtendedError (\p -> SetProgs p : []) (\(w,p,t) -> SetWater w : SetProgs p : SetTrees t : [])
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

withExtendedError :: Bifoldable p => (a -> [Action]) -> (b -> [Action]) -> p (ErrorLogLine, a) b -> [Action]
withExtendedError f = bifoldMap (\(err, a) -> SetE err : f a)

pumpWater :: MyState -> MyState
pumpWater state
  = handleActions
  $ addActions state
  $ (\w -> SetWater w : [])
  $ PBL.pumpWater state

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