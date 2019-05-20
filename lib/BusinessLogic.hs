{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Data.Text (concat, pack)

import Mod

buyHelper :: Seconds -> HelperPrice -> Paperclips -> Helpers -> [ErrorLogLine] -> Either [ErrorLogLine] (Helpers, Paperclips)
buyHelper s price p helpers log =
  if unHelperPrice price > p
    then Left $ addToErrorLog (lineNeedMorePaperclips s) log
    else Right (succ helpers, decPaperclipsWith price p)

decPaperclipsWith :: HelperPrice -> Paperclips -> Paperclips
decPaperclipsWith price paperclips = paperclips - (unHelperPrice price)

addToErrorLog :: ErrorLogLine -> [ErrorLogLine] -> [ErrorLogLine]
addToErrorLog new existing = existing ++ [new]

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

researchAdvancedHelper :: Seconds -> Paperclips -> AdvancedHelperPrice -> ResearchProgress -> Duration -> [ErrorLogLine] -> Either [ErrorLogLine] (Paperclips, ResearchProgress)
researchAdvancedHelper s p price progress duration errs =
  case ((unAdvancedHelperPrice price > p), progress) of
    (True, NotResearched) -> Left $ addToErrorLog (ErrorLogLine "Not enough paperclips") errs
    (False, NotResearched) -> Right (decPaperclipsWith' (unAdvancedHelperPrice price) p, startResearch duration)
    (_, ResearchInProgress x) -> Left $ addToErrorLog (ErrorLogLine "Already in progress") errs
    (_, ResearchDone) -> Left $ addToErrorLog (ErrorLogLine "Already done") errs

decPaperclipsWith' :: Paperclips -> Paperclips -> Paperclips
decPaperclipsWith' price paperclips = paperclips - price

plantASeed :: Seconds -> TreePrice -> TreeSeeds -> Trees -> [ErrorLogLine] -> Either [ErrorLogLine] (TreeSeeds, Trees)
plantASeed s price seeds trees errs =
  if unTreePrice price > seeds
    then Left $ addToErrorLog (lineNeedMoreSeeds s) errs
    else Right (decSeedsWith price seeds, succ trees)

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

decSeedsWith :: TreePrice -> TreeSeeds -> TreeSeeds
decSeedsWith price seeds = seeds - (unTreePrice price)