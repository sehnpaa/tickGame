module Lib
  ( Initial.getInitialState
  , module Config
  , module Elements
  , module Lib
  , module Mod
  , module Resources
  )
where

import           Data.Bifoldable                ( Bifoldable
                                                , bifoldMap
                                                )
import           Control.Lens                   ( over
                                                , set
                                                , view
                                                )

import           Config
import           Elements
import qualified Initial                       as Initial
import           Mod
import           Resources
import qualified PathedBusinessLogic           as PBL

nextTick :: (Enum a, Num a, Ord a, Show a) => MyState a -> MyState a
nextTick = handleActions . addSecond . helperWork . seedWork . researchWork

handleActions :: MyState a -> MyState a
handleActions state =
  let as = view actions state in set actions [] $ foldr applyAction state as

applyAction :: Action a -> MyState a -> MyState a
applyAction (SetP p) state =
  set (resources . elements . elementPaperclips . count) p state
applyAction (SetH h) state =
  set (resources . elements . elementHelpers . count) h state
applyAction (SetE err) state = over errorLog (\errs -> err : errs) state
applyAction (SetR r) state =
  set (researchAreas . advancedHelperResearch . researchCompProgress) r state
applyAction (SetTreeSeeds s) state =
  set (resources . elements . elementTreeSeeds . count) s state
applyAction (SetTrees t) state =
  set (resources . elements . elementTrees . count) t state
applyAction (SetAdvancedHelperResearchProgress p) state =
  set (researchAreas . advancedHelperResearch . researchCompProgress) p state
applyAction (SetHelperInc i) state =
  set (config . constants . helperInc) i state
applyAction (SetProgs ps) state =
  set (resources . elements . elementTreeSeeds . count . progs) ps state
applyAction (SetWater w) state =
  set (resources . elements . elementWater . count) w state

helperWork :: (Num a, Ord a) => MyState a -> MyState a
helperWork state = addActions state $ (singleton . SetP) $ PBL.helperWork state

singleton :: a -> [a]
singleton = (: [])

researchWork :: (Eq a, Num a) => MyState a -> MyState a
researchWork state =
  handleActions
    $ addActions state
    $ (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    $ PBL.researchWork state

seedWork :: (Num a, Ord a, Show a) => MyState a -> MyState a
seedWork state =
  addActions state
    $ withExtendedError
        (\p -> SetProgs p : [])
        (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
    $ PBL.seedWork state

addSecond :: (Enum a) => MyState a -> MyState a
addSecond = over seconds succ

createPaperclip :: (Enum a, Ord a) => MyState a -> MyState a
createPaperclip state =
  handleActions $ addActions state $ (\p -> SetP p : []) $ PBL.createPaperclip
    state

buyHelper :: (Enum a, Num a, Ord a, Show a) => MyState a -> MyState a
buyHelper state =
  handleActions
    $ addActions state
    $ withError (\(h, p) -> SetH h : SetP p : [])
    $ PBL.buyHelper state

withError :: Bifoldable p => (b -> [Action a]) -> p ErrorLogLine b -> [Action a]
withError = bifoldMap (singleton . SetE)

withExtendedError
  :: Bifoldable p
  => (a -> [Action x])
  -> (b -> [Action x])
  -> p (ErrorLogLine, a) b
  -> [Action x]
withExtendedError f = bifoldMap (\(err, a) -> SetE err : f a)

pumpWater :: (Enum a, Num a, Ord a) => MyState a -> MyState a
pumpWater state =
  handleActions $ addActions state $ (\w -> SetWater w : []) $ PBL.pumpWater
    state

addActions :: MyState a -> [Action a] -> MyState a
addActions state newActions = over actions (\as -> newActions ++ as) state

researchAdvancedHelper :: (Num a, Ord a, Show a) => MyState a -> MyState a
researchAdvancedHelper state =
  handleActions
    $ addActions state
    $ withError (\(p, r) -> SetP p : SetR r : [])
    $ PBL.researchAdvancedHelper state

plantASeed :: (Num a, Ord a, Show a) => MyState a -> MyState a
plantASeed state =
  handleActions
    $ addActions state
    $ withError (\s -> SetTreeSeeds s : [])
    $ PBL.plantASeed state

buyASeed :: (Num a, Ord a, Show a) => MyState a -> MyState a
buyASeed state =
  handleActions
    $ addActions state
    $ withError (\(s, p) -> SetTreeSeeds s : SetP p : [])
    $ PBL.buyASeed state

setStarted :: MyState a -> MyState a
setStarted = over isStarted (const $ IsStarted True)
