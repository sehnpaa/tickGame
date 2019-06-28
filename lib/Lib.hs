module Lib
  ( Initial.getInitialState
  , module Config
  , module Elements
  , module Lib
  , module State
  , module Resources
  )
where

import           Control.Lens                   ( over
                                                , set
                                                , view
                                                )

import           Config
import           Elements
import qualified Initial                       as Initial
import           State
import           Resources
import qualified PathedBusinessLogic           as PBL
import           Utils

nextTick :: (Enum a, Num a, Ord a, Show a) => State a -> State a
nextTick = handleActions . addSecond . helperWork . seedWork . researchWork

handleActions :: State a -> State a
handleActions state =
  let as = view actions state in set actions [] $ foldr applyAction state as

helperWork :: (Num a, Ord a) => State a -> State a
helperWork state = addActions state $ (singleton . SetP) $ PBL.helperWork state

researchWork :: (Eq a, Num a) => State a -> State a
researchWork state =
  handleActions
    $ addActions state
    $ (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    $ PBL.researchWork state

seedWork :: (Num a, Ord a, Show a) => State a -> State a
seedWork state =
  addActions state
    $ withExtendedError
        SetE
        (\p -> SetProgs p : [])
        (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
    $ PBL.seedWork state

addSecond :: (Enum a) => State a -> State a
addSecond = over seconds succ

createPaperclip :: (Enum a, Ord a) => State a -> State a
createPaperclip state =
  handleActions $ addActions state $ (\p -> SetP p : []) $ PBL.createPaperclip
    state

buyHelper :: (Enum a, Num a, Ord a, Show a) => State a -> State a
buyHelper state =
  handleActions
    $ addActions state
    $ withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : [])
    $ PBL.buyHelper state

pumpWater :: (Enum a, Num a, Ord a) => State a -> State a
pumpWater state =
  handleActions $ addActions state $ (\w -> SetWater w : []) $ PBL.pumpWater
    state

addActions :: State a -> [Action a] -> State a
addActions state newActions = over actions (\as -> newActions ++ as) state

researchAdvancedHelper :: (Num a, Ord a, Show a) => State a -> State a
researchAdvancedHelper state =
  handleActions
    $ addActions state
    $ withError SetE (\(p, r) -> SetP p : SetR r : [])
    $ PBL.researchAdvancedHelper state

plantASeed :: (Num a, Ord a, Show a) => State a -> State a
plantASeed state =
  handleActions
    $ addActions state
    $ withError SetE (\s -> SetTreeSeeds s : [])
    $ PBL.plantASeed state

buyASeed :: (Num a, Ord a, Show a) => State a -> State a
buyASeed state =
  handleActions
    $ addActions state
    $ withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])
    $ PBL.buyASeed state

generateEnergy :: (Enum a, Num a, Ord a, Show a) => State a -> State a
generateEnergy state =
  handleActions
    $ addActions state
    $ (\e -> SetEnergy e : [])
    $ PBL.generateEnergy state

setStarted :: State a -> State a
setStarted = over isStarted (const $ IsStarted True)
