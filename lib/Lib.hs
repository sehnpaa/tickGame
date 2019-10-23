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
import           Control.Monad.Reader           ( runReader )
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
    . (\st -> performActions stateActions applyAction st (view stateActions st))
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
  performActions stateActions applyAction st
    . (singleton . SetP)
    . uncurryN BL.run
    . PBL.run
    $ st

helperWork :: (Num a, Ord a) => State a -> State a
helperWork st =
  performActions stateActions applyAction st
    . (singleton . SetP)
    . uncurryN BL.helperWork
    . PBL.helperWork
    $ st

researchWork :: (Eq a, Num a) => State a -> State a
researchWork st =
  performActions stateActions applyAction st
    . (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    . uncurryN BL.researchWork
    . PBL.researchWork
    $ st

seedWork :: (Num a, Ord a, Show a) => State a -> State a
seedWork st =
  performActions stateActions applyAction st
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

createPaperclip :: (Ord a, Enum a, HasState s a, HasPaperclips s a, HasStorageOfPaperclips s a) => s -> s
createPaperclip st =
  performActions stateActions applyAction st
    . (\p -> SetP p : [])
    . runReader BL.createPaperclip
    $ st

performActions
  :: (Foldable t) => ASetter' s [a] -> (a -> s -> s) -> s -> t a -> s
performActions l f st = set l [] . foldr f st

buyHelper :: (Enum a, Num a, Ord a, Show a, HasState s a) => s -> s
buyHelper st =
  performActions stateActions applyAction st
    . (withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : []))
    . BL.buyHelper getSeconds
                   getHelpersManuallyCostEnergy
                   getHelpersManuallyCostPaperclips
                   getHelpersManuallyEnergyErrorMessage
                   getHelpersManuallyPaperclipsErrorMessage
                   getPaperclipCount
                   getEnergyCount
                   getHelperCount
                   mkErrorLogLine
    $ st

buyASeed :: (Num a, Ord a, Show a) => State a -> State a
buyASeed st =
  performActions stateActions applyAction st
    . withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])
    . uncurryN BL.buyASeed
    . PBL.buyASeed
    $ st

extendStorage :: (Num a, Ord a, Show a) => State a -> State a
extendStorage st =
  performActions stateActions applyAction st
    . withError SetE (\(s, w) -> SetStorageOfPaperclips s : SetWood w : [])
    . uncurryN BL.extendStorage
    . PBL.extendStorage
    $ st

generateEnergy :: (Enum a, Ord a, Num a, Show a) => State a -> State a
generateEnergy st =
  performActions stateActions applyAction st
    . (\e -> SetEnergy e : [])
    . BL.generateEnergy getEnergyCount
    $ st

researchAdvancedHelper :: (Num a, Ord a, Show a) => State a -> State a
researchAdvancedHelper st =
  performActions stateActions applyAction st
    . withError SetE (\(p, r) -> SetP p : SetR r : [])
    . uncurryN BL.researchAdvancedHelper
    . PBL.researchAdvancedHelper
    $ st

plantASeed :: (HasTreeSeeds s a, HasDurationTreeSeeds s a,
                 HasSeconds s a, Show a, Ord a, Num a, HasState s a) =>
                s -> s
plantASeed st =
  performActions stateActions applyAction st
    . withError SetE (\s -> SetTreeSeeds s : [])
    . runReader BL.plantASeed
    $ st

pumpWater :: (Enum a, Num a, Ord a) => State a -> State a
pumpWater st =
  performActions stateActions applyAction st
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
