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

import           Config
import           Elements
import qualified Initial                       as Initial
import           Seconds
import           Source
import           State
import qualified StateStep                     as SS
import           StateStep                      ( addActions
                                                , runS
                                                )
import           Resources
import qualified PathedBusinessLogic           as PBL
import           Utils

nextTick :: (Enum a, Integral a, Num a, Ord a, Show a) => State a -> State a
nextTick =
  handleActions . helperWork . seedWork . researchWork . runCode . addSecond

runCode :: (Eq a, Integral a, Num a) => State a -> State a
runCode state = addActions state $ (singleton . SetP) $ PBL.run state

handleActions :: Num a => State a -> State a
handleActions state =
  let as = view actions state in set actions [] $ foldr applyAction state as

helperWork :: (Num a, Ord a) => State a -> State a
helperWork = runS SS.helperWork

researchWork :: (Eq a, Num a) => State a -> State a
researchWork = handleActions . runS SS.researchWork

seedWork :: (Num a, Ord a, Show a) => State a -> State a
seedWork = runS SS.seedWork

addSecond :: (Enum a) => State a -> State a
addSecond = over seconds succ

createPaperclip :: (Enum a, Num a, Ord a) => State a -> State a
createPaperclip = handleActions . runS SS.createPaperclip

buyHelper :: (Enum a, Num a, Ord a, Show a) => State a -> State a
buyHelper = handleActions . runS SS.buyHelper

buyASeed :: (Num a, Ord a, Show a) => State a -> State a
buyASeed = handleActions . runS SS.buyASeed

generateEnergy :: (Enum a, Num a, Ord a, Show a) => State a -> State a
generateEnergy = handleActions . runS SS.generateEnergy

researchAdvancedHelper :: (Num a, Ord a, Show a) => State a -> State a
researchAdvancedHelper = handleActions . runS SS.researchAdvancedHelper

plantASeed :: (Num a, Ord a, Show a) => State a -> State a
plantASeed = handleActions . runS SS.plantASeed

pumpWater :: (Enum a, Num a, Ord a) => State a -> State a
pumpWater = handleActions . runS SS.pumpWater


compile :: Text -> State a -> State a
compile text = set (source . sourceText) (SourceText text) . set
  (source . sourceStatus)
  (case parse text :: Either CustomParseError (Expr Integer) of
    Left  (CPE s)        -> SourceStatus $ pack $ "Not runnable:\n" ++ s
    Left  NothingToParse -> SourceStatus $ pack $ "Nothing to parse."
    Right _              -> SourceStatus $ pack "OK!"
  )

setStarted :: State a -> State a
setStarted = set isStarted (IsStarted True)
