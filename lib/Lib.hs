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

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import qualified Initial                       as Initial
import           Seconds
import           Source
import           State
import           Resources

import           Utils

nextTick
  :: ( Enum a
     , Integral a
     , Num a
     , Ord a
     , Show a
     , HasSeconds s a
     , HasHelperInc s a
     , HasHelpers s a
     , HasPaperclips s a
     , HasResearchProgress s a
     , HasSource s a
     , HasStorageOfPaperclips s a
     , HasState s a
     , HasTreeSeedCostPerTick s a
     , HasTreeSeeds s a
     , HasTrees s a
     , HasWater s a
     )
  => s
  -> s
nextTick =
  saveSnapshot
    . (\st -> performActions (stateActions . unActions) applyAction st (view (stateActions . unActions) st))
    . helperWork
    . seedWork
    . researchWork
    . runCode
    . addSecond

saveSnapshot :: (HasState s a) => s -> s
saveSnapshot st =
  let r = view stateResources st
  in  over (stateSnapshots . zipper) (insert r) st

runCode
  :: ( Integral a
     , HasPaperclips s a
     , HasSeconds s a
     , HasSource s a
     , HasState s a
     , HasStorageOfPaperclips s a
     )
  => s
  -> s
runCode st =
  performActions (stateActions . unActions) applyAction st
    . (singleton . SetP)
    . runReader BL.run
    $ st

helperWork
  :: ( Num a
     , Ord a
     , HasHelperInc s a
     , HasHelpers s a
     , HasPaperclips s a
     , HasState s a
     , HasStorageOfPaperclips s a
     )
  => s
  -> s
helperWork st =
  performActions (stateActions . unActions) applyAction st
    . (singleton . SetP)
    . runReader BL.helperWork
    $ st

researchWork
  :: (Eq a, Num a, HasHelperInc s a, HasResearchProgress s a, HasState s a)
  => s
  -> s
researchWork st =
  performActions (stateActions . unActions) applyAction st
    . (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    . runReader BL.researchWork
    $ st

seedWork
  :: ( Num a
     , Ord a
     , Show a
     , HasSeconds s a
     , HasState s a
     , HasTreeSeedCostPerTick s a
     , HasTreeSeeds s a
     , HasTrees s a
     , HasWater s a
     )
  => s
  -> s
seedWork st =
  performActions (stateActions . unActions) applyAction st
    . (withExtendedError
        SetE
        (\p -> SetProgs p : [])
        (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
      )
    . runReader BL.seedWork
    $ st

addSecond :: (Enum a, HasState s a) => s -> s
addSecond = over stateSeconds succ

createPaperclip
  :: ( Ord a
     , Enum a
     , HasState s a
     , HasActions s a
     , HasPaperclips s a
     , HasStorageOfPaperclips s a
     )
  => s
  -> s
createPaperclip st =
  performActions (stateActions . unActions) applyAction st
    . (\p -> SetP p : [])
    . runReader BL.createPaperclip
    $ st

performActions
  :: (Foldable t) => ASetter' s [a] -> (a -> s -> s) -> s -> t a -> s
performActions l f st = set l [] . foldr f st

buyHelper
  :: ( Enum a
     , Num a
     , Ord a
     , Show a
     , HasCostEnergyPaperclips s a
     , HasEnergy s a
     , HasEnergyErrorMessage s
     , HasHelpers s a
     , HasPaperclips s a
     , HasPaperclipsErrorMessage s
     , HasSeconds s a
     , HasState s a
     )
  => s
  -> s
buyHelper st =
  performActions (stateActions . unActions) applyAction st
    . (withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : []))
    . runReader BL.buyHelper
    $ st

buyASeed
  :: ( HasSeconds s a
     , HasBuyTreeSeeds s a
     , HasPaperclips s a
     , HasTreeSeeds s a
     , Show a
     , Ord a
     , Num a
     , HasState s a
     )
  => s
  -> s
buyASeed st =
  performActions (stateActions . unActions) applyAction st
    . withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])
    . runReader BL.buyASeed
    $ st

extendStorage
  :: ( Num a
     , Ord a
     , Show a
     , HasSeconds s a
     , HasState s a
     , HasStorageManually s a
     , HasStorageOfPaperclips s a
     , HasWood s a
     )
  => s
  -> s
extendStorage st =
  performActions (stateActions . unActions) applyAction st
    . withError SetE (\(s, w) -> SetStorageOfPaperclips s : SetWood w : [])
    . runReader BL.extendStorage
    $ st

generateEnergy
  :: (Enum a, Ord a, Num a, Show a, HasEnergy s a, HasState s a) => s -> s
generateEnergy st =
  performActions (stateActions . unActions) applyAction st
    . (\e -> SetEnergy e : [])
    . runReader BL.generateEnergy
    $ st

researchAdvancedHelper
  :: ( Num a
     , Ord a
     , Show a
     , HasAdvancedHelperPriceInPaperclips s a
     , HasPaperclips s a
     , HasResearchComp s a
     , HasSeconds s a
     , HasState s a
     )
  => s
  -> s
researchAdvancedHelper st =
  performActions (stateActions . unActions) applyAction st
    . withError SetE (\(p, r) -> SetP p : SetR r : [])
    . runReader BL.researchAdvancedHelper
    $ st

plantASeed
  :: ( HasTreeSeeds s a
     , HasDurationTreeSeeds s a
     , HasSeconds s a
     , Show a
     , Ord a
     , Num a
     , HasState s a
     )
  => s
  -> s
plantASeed st =
  performActions (stateActions . unActions) applyAction st
    . withError SetE (\s -> SetTreeSeeds s : [])
    . runReader BL.plantASeed
    $ st

pumpWater
  :: (Enum a, Num a, Ord a, HasState s a, HasWater s a, HasWaterTank s a)
  => s
  -> s
pumpWater st =
  performActions (stateActions . unActions) applyAction st
    . (\w -> SetWater w : [])
    . runReader BL.pumpWater
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
