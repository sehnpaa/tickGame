{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Data.Text (concat, pack)

import Mod

helperWork :: Paperclips -> Helpers -> HelperInc -> Storage -> Paperclips
helperWork p h inc storage = Paperclips $ min (unStorage storage) $ (unPaperclips $ addHelperWork inc h p)

addHelperWork :: HelperInc -> Helpers -> Paperclips -> Paperclips
addHelperWork inc h p = Paperclips $ (unPaperclips p) + (unHelpers h) * (unHelpers $ unHelperInc inc)

buyHelper :: Seconds -> HelperPrice -> Paperclips -> Helpers -> Either ErrorLogLine (Helpers, Paperclips)
buyHelper s (HelperPrice price) p h =
  if price > p
    then Left (lineNeedMorePaperclips s)
    else Right (succ h, decPaperclipsWith price p)

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

researchAdvancedHelper :: Seconds -> Paperclips -> AdvancedHelperPrice -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips, ResearchProgress)
researchAdvancedHelper s p (AdvancedHelperPrice price) progress duration =
  case (price > p, progress) of
    (True, NotResearched) -> Left $ ErrorLogLine "Not enough paperclips"
    (False, NotResearched) -> Right (decPaperclipsWith price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ ErrorLogLine "Already in progress"
    (_, ResearchDone) -> Left $ ErrorLogLine "Already done"

decPaperclipsWith :: Paperclips -> Paperclips -> Paperclips
decPaperclipsWith price p = p - price

plantASeed :: Seconds -> TreePrice -> TreeSeeds -> Trees -> Either ErrorLogLine (TreeSeeds, Trees)
plantASeed s price seeds t =
  if unTreePrice price > seeds
    then Left $ lineNeedMoreSeeds s
    else Right (decSeedsWith price seeds, succ t)

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

decSeedsWith :: TreePrice -> TreeSeeds -> TreeSeeds
decSeedsWith price seeds = seeds - (unTreePrice price)