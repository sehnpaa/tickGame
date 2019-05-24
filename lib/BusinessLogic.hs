{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Control.Lens
import Data.Text (concat, pack)

import Mod

helperWork :: Paperclips -> Helpers -> HelperInc -> Storage -> Paperclips
helperWork p h inc storage = Paperclips $ min (unStorage storage) $ (unPaperclips $ addHelperWork inc h p)

addHelperWork :: HelperInc -> Helpers -> Paperclips -> Paperclips
addHelperWork inc h p = Paperclips $ (unPaperclips p) + (unHelpers h) * (unHelpers $ unHelperInc inc)

researchWork :: ResearchProgress -> HelperInc -> (ResearchProgress, HelperInc)
researchWork progress c =
  case progress of
    NotResearched -> (progress, c)
    ResearchInProgress 1 -> (ResearchDone, incHelperIncWith c c)
    ResearchInProgress n -> (ResearchInProgress (n-1), c)
    ResearchDone -> (progress, c)

incHelperIncWith :: HelperInc -> HelperInc -> HelperInc
incHelperIncWith = withIso (isoHelpers . isoHelperInc) (\_ elim inc -> under (isoHelpers . isoHelperInc) (\h -> h + elim inc))

isoHelperInc :: (Profunctor p, Functor f) => p HelperInc (f HelperInc) -> p Helpers (f Helpers)
isoHelperInc = iso HelperInc unHelperInc

buyHelper :: Seconds -> HelperPrice -> Paperclips -> Helpers -> Either ErrorLogLine (Helpers, Paperclips)
buyHelper s price p h =
  if unHelperPrice price > p
    then Left (lineNeedMorePaperclips s)
    else Right (succ h, decPaperclipsWith price p)

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

researchAdvancedHelper :: Seconds -> Paperclips -> AdvancedHelperPrice -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips, ResearchProgress)
researchAdvancedHelper s p price progress duration =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ ErrorLogLine "Not enough paperclips"
    (False, NotResearched) -> Right (decPaperclipsWith' price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ ErrorLogLine "Already in progress"
    (_, ResearchDone) -> Left $ ErrorLogLine "Already done"

decPaperclipsWith :: HelperPrice -> Paperclips -> Paperclips
decPaperclipsWith = withIso (isoPaperclips . isoHelperPrice) (\_ elim price -> under isoPaperclips (\p -> p - elim price))

decPaperclipsWith' :: AdvancedHelperPrice -> Paperclips -> Paperclips
decPaperclipsWith' = withIso (isoPaperclips . isoAdvancedHelperPrice) (\_ elim price -> under isoPaperclips (\p -> p - elim price))

plantASeed :: Seconds -> TreePrice -> TreeSeeds -> Trees -> Either ErrorLogLine (TreeSeeds, Trees)
plantASeed s price seeds t =
  if unTreePrice price > seeds
    then Left $ lineNeedMoreSeeds s
    else Right (decSeedsWith price seeds, succ t)

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

decSeedsWith :: TreePrice -> TreeSeeds -> TreeSeeds
decSeedsWith = withIso (isoTreeSeeds . isoTreePrice) (\_ elim price -> under isoTreeSeeds (\s -> s - elim price))

isoHelperPrice :: (Profunctor p, Functor f) => p HelperPrice (f HelperPrice) -> p Paperclips (f Paperclips)
isoHelperPrice = iso HelperPrice unHelperPrice

isoHelpers :: (Profunctor p, Functor f) => p Helpers (f Helpers) -> p Integer (f Integer)
isoHelpers = iso Helpers unHelpers

isoAdvancedHelperPrice :: (Profunctor p, Functor f) => p AdvancedHelperPrice (f AdvancedHelperPrice) -> p Paperclips (f Paperclips)
isoAdvancedHelperPrice = iso AdvancedHelperPrice unAdvancedHelperPrice

isoTreeSeeds :: (Profunctor p, Functor f) => p TreeSeeds (f TreeSeeds) -> p Integer (f Integer)
isoTreeSeeds = iso TreeSeeds unTreeSeeds

isoTreePrice :: (Profunctor p, Functor f) => p TreePrice (f TreePrice) -> p TreeSeeds (f TreeSeeds)
isoTreePrice = iso TreePrice unTreePrice

createPaperclip :: Paperclips -> Paperclips
createPaperclip = under isoPaperclips succ

isoPaperclips :: (Profunctor p, Functor f) => p Paperclips (f Paperclips) -> p Integer (f Integer)
isoPaperclips = iso Paperclips unPaperclips