module Lib
  ( Initial.getInitialState
  , module Config
  , module Elements
  , module Lib
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
import           Source
import           State
import           Resources
import qualified PathedBusinessLogic           as PBL
import           Utils

nextTick :: (Enum a, Num a, Ord a, Show a) => State a -> State a
nextTick =
  handleActions . helperWork . seedWork . researchWork . runCode . addSecond

runCode :: (Eq a, Num a) => State a -> State a
runCode state =
  addActions state $ run (view seconds state) (view (source . sourceExpr) state)

run :: (Eq a, Num a) => Seconds a -> Maybe (Expr a) -> [Action a]
run _ Nothing     = []
run s (Just expr) = exprToActions s expr

exprToActions :: (Eq a, Num a) => Seconds a -> (Expr a) -> [Action a]
exprToActions (Seconds s) (SyncPaperclipsWithSeconds s' p) =
  if s == s' then [SetP p] else []

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

compile :: Text -> State Integer -> State Integer
compile text = set source $ case ff text of
  Nothing -> Source text (pack "Not ok.") Nothing
  Just x  -> Source text (pack "OK!") (Just x)

setStarted :: State a -> State a
setStarted = over isStarted (const $ IsStarted True)
