{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Lens                   ( over
                                                , set
                                                , view
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import qualified Initial                       as Initial
import qualified PathedBusinessLogic           as PBL
import           Seconds
import           Source
import           State
import           Resources
import           Data.Tuple.Curry
import           Utils

nextTick :: (Enum a, Integral a, Num a, Ord a, Show a) => State a -> State a
nextTick =
  handleActions
    . saveSnapshot
    . helperWork
    . seedWork
    . researchWork
    . runCode
    . addSecond

saveSnapshot :: State a -> State a
saveSnapshot state =
  let r = view resources state
  in  over snapshots (\old -> Snapshots $ r : unSnapshots old) state

runCode :: (Eq a, Integral a, Num a) => State a -> State a
runCode state =
  addActions state . (singleton . SetP) . uncurryN BL.run . PBL.run $ state

handleActions :: Num a => State a -> State a
handleActions state =
  let as = view actions state in set actions [] $ foldr applyAction state as

helperWork :: (Num a, Ord a) => State a -> State a
helperWork state =
  addActions state
    . (singleton . SetP)
    . uncurryN BL.helperWork
    . PBL.helperWork
    $ state

researchWork :: (Eq a, Num a) => State a -> State a
researchWork state =
  handleActions
    . addActions state
    . (\(p, h) -> SetAdvancedHelperResearchProgress p : SetHelperInc h : [])
    . uncurryN BL.researchWork
    . PBL.researchWork
    $ state

seedWork :: (Num a, Ord a, Show a) => State a -> State a
seedWork state =
  addActions state
    . (withExtendedError
        SetE
        (\p -> SetProgs p : [])
        (\(w, p, t) -> SetWater w : SetProgs p : SetTrees t : [])
      )
    . uncurryN BL.seedWork
    . PBL.seedWork
    $ state

addSecond :: (Enum a) => State a -> State a
addSecond = over seconds succ

createPaperclip :: (Enum a, Num a, Ord a) => State a -> State a
createPaperclip state =
  handleActions
    . addActions state
    . (\p -> SetP p : [])
    . uncurryN BL.createPaperclip
    . PBL.createPaperclip
    $ state

buyHelper :: (Enum a, Num a, Ord a, Show a) => State a -> State a
buyHelper state =
  handleActions
    . addActions state
    . (withError SetE (\(h, e, p) -> SetH h : SetEnergy e : SetP p : []))
    . uncurryN BL.buyHelper
    . PBL.buyHelper
    $ state

buyASeed :: (Num a, Ord a, Show a) => State a -> State a
buyASeed state =
  handleActions
    . addActions state
    . withError SetE (\(s, p) -> SetTreeSeeds s : SetP p : [])
    . uncurryN BL.buyASeed
    . PBL.buyASeed
    $ state

extendStorage :: (Num a, Ord a, Show a) => State a -> State a
extendStorage state =
  handleActions
    . addActions state
    . withError SetE (\(s, w) -> SetStorage s : SetWood w : [])
    . uncurryN BL.extendStorage
    . PBL.extendStorage2
    $ state

generateEnergy :: (Enum a, Ord a, Num a, Show a) => State a -> State a
generateEnergy s =
  handleActions
    . addActions s
    . (\e -> SetEnergy e : [])
    . uncurryN BL.generateEnergy
    . PBL.generateEnergy
    $ s

researchAdvancedHelper :: (Num a, Ord a, Show a) => State a -> State a
researchAdvancedHelper state =
  handleActions
    . addActions state
    . withError SetE (\(p, r) -> SetP p : SetR r : [])
    . uncurryN BL.researchAdvancedHelper
    . PBL.researchAdvancedHelper
    $ state

plantASeed :: (Num a, Ord a, Show a) => State a -> State a
plantASeed state =
  handleActions
    . addActions state
    . withError SetE (\s -> SetTreeSeeds s : [])
    . uncurryN BL.plantASeed
    . PBL.plantASeed
    $ state

pumpWater :: (Enum a, Num a, Ord a) => State a -> State a
pumpWater state =
  handleActions
    . addActions state
    . (\w -> SetWater w : [])
    . uncurryN BL.pumpWater
    . PBL.pumpWater
    $ state

compile :: Text -> State a -> State a
compile text = set (source . sourceText) (SourceText text) . set
  (source . sourceStatus)
  (case parse text :: Either CustomParseError (Expr Integer) of
    Left  (CPE s)        -> SourceStatus $ pack $ "Not runnable:\n" ++ s
    Left  NothingToParse -> SourceStatus $ pack $ "Nothing to parse."
    Right _              -> SourceStatus $ pack "OK!"
  )

setStarted :: Bool -> State a -> State a
setStarted True =
  set (events . eventStart . eventStartButtonData . buttonTitle)
      (ButtonTitle "Pause")
    . set isStarted (IsStarted True)
setStarted False =
  set (events . eventStart . eventStartButtonData . buttonTitle)
      (ButtonTitle "Start")
    . set isStarted (IsStarted False)
