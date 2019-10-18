{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Lib
  ( Initial.getInitialState
  , module Config
  , module Elements
  , module Lib
  , module Seconds
  , module Source
  , module State
  , module Resources
  )
where

import           Control.Lens                   ( ASetter'
                                                , over
                                                , set
                                                , view
                                                )
import           Data.List.Zipper               ( insert
                                                , left
                                                , right
                                                , safeCursor
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Tuple.Curry

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import qualified Initial                       as Initial
import           Path
import qualified PathedBusinessLogic           as PBL
import           Seconds
import           Source
import           State
import           Resources

import           Utils

nextTick :: (Enum a, Integral a, Num a, Ord a, Show a) => State a -> State a
nextTick =
  saveSnapshot
    . handleActions
    . helperWork
    . seedWork
    . researchWork
    . runCode
    . addSecond

saveSnapshot :: State a -> State a
saveSnapshot st =
  let r = view stateResources st
  in  over (stateSnapshots . zipper) (insert r) st

runCode :: (Eq a, Integral a, Num a) => State a -> State a
runCode st =
  addActions st . (singleton . SetP) . uncurryN BL.run . PBL.run $ st

handleActions :: HasState s a => s -> s
handleActions st =
  let as = view stateActions st
  in  set stateActions [] $ foldr applyAction st as

helperWork :: (Num a, Ord a) => State a -> State a
helperWork st =
  addActions st
    . (singleton . SetP)
    . uncurryN BL.helperWork
    . PBL.helperWork
    $ st

researchWork :: (Eq a, Num a) => State a -> State a
researchWork st =
  handleActions
    . addActions st
    . (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    . uncurryN BL.researchWork
    . PBL.researchWork
    $ st

seedWork :: (Num a, Ord a, Show a) => State a -> State a
seedWork st =
  addActions st
    . (withExtendedError
        SetE
        (\p -> SetProgs p : [])
        (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
      )
    . uncurryN BL.seedWork
    . PBL.seedWork
    $ st

addSecond :: (Enum a) => State a -> State a
addSecond = over stateSeconds succ

createPaperclip :: (Ord a, Enum a, HasState s a) => s -> s
createPaperclip st =
  performActions stateActions applyAction st
    . (\p -> SetP p : [])
    . BL.createPaperclip getPaperclipCount getStorage
    $ st

performActions
  :: (Foldable t) => ASetter' s [a] -> (a -> s -> s) -> s -> t a -> s
performActions l f st = set l [] . foldr f st

buyHelper :: (Enum a, Num a, Ord a, Show a) => State a -> State a
buyHelper st =
  handleActions
    . addActions st
    . (withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : []))
    . uncurryN BL.buyHelper
    . PBL.buyHelper
    $ st

buyASeed :: (Num a, Ord a, Show a) => State a -> State a
buyASeed st =
  handleActions
    . addActions st
    . withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])
    . uncurryN BL.buyASeed
    . PBL.buyASeed
    $ st

extendStorage :: (Num a, Ord a, Show a) => State a -> State a
extendStorage st =
  handleActions
    . addActions st
    . withError SetE (\(s, w) -> SetStorage s : SetWood w : [])
    . uncurryN BL.extendStorage
    . PBL.extendStorage
    $ st

generateEnergy :: (Enum a, Ord a, Num a, Show a) => State a -> State a
generateEnergy st =
  handleActions
    . addActions st
    . (\e -> SetEnergy e : [])
    . uncurryN BL.generateEnergy
    . PBL.generateEnergy
    $ st

researchAdvancedHelper :: (Num a, Ord a, Show a) => State a -> State a
researchAdvancedHelper st =
  handleActions
    . addActions st
    . withError SetE (\(p, r) -> SetP p : SetR r : [])
    . uncurryN BL.researchAdvancedHelper
    . PBL.researchAdvancedHelper
    $ st

plantASeed :: (Num a, Ord a, Show a) => State a -> State a
plantASeed st =
  handleActions
    . addActions st
    . withError SetE (\s -> SetTreeSeeds s : [])
    . uncurryN BL.plantASeed
    . PBL.plantASeed
    $ st

pumpWater :: (Enum a, Num a, Ord a) => State a -> State a
pumpWater st =
  handleActions
    . addActions st
    . (\w -> SetWater w : [])
    . uncurryN BL.pumpWater
    . PBL.pumpWater
    $ st

compile :: Text -> State a -> State a
compile text = set (stateSource . sourceText) (SourceText text) . set
  (stateSource . sourceStatus)
  (case parse text :: Either CustomParseError (Expr Integer) of
    Left  (CPE s)        -> SourceStatus $ pack $ "Not runnable:\n" ++ s
    Left  NothingToParse -> SourceStatus $ pack $ "Nothing to parse."
    Right _              -> SourceStatus $ pack "OK!"
  )

previousSnapshot :: State a -> State a
previousSnapshot = over (stateSnapshots . zipper) right

nextSnapshot :: State a -> State a
nextSnapshot = over (stateSnapshots . zipper) left

applySnapshot :: State a -> State a
applySnapshot st = over
  stateResources
  (\old -> case safeCursor (view (stateSnapshots . zipper) st) of
    Nothing  -> old
    Just new -> new
  )
  st

setStarted :: Bool -> State a -> State a
setStarted True =
  saveSnapshot
    . set
        (stateEvents . eventsEventStart . eventStartButtonData . buttonTitle)
        (ButtonTitle "Pause")
    . set stateIsStarted (IsStarted True)
setStarted False =
  clearAllFutureSnapshots
    . set
        (stateEvents . eventsEventStart . eventStartButtonData . buttonTitle)
        (ButtonTitle "Start")
    . set stateIsStarted (IsStarted False)
