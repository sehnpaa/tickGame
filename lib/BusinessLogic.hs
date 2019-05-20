{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Data.Text (concat, pack)

import Mod

buyHelper :: Seconds -> HelperInc -> HelperPrice -> Paperclips -> Helpers -> [ErrorLogLine] -> Either [ErrorLogLine] (Helpers, Paperclips)
buyHelper s (HelperInc inc) (HelperPrice price) p helpers log =
  if price > p
    then Left $ addToErrorLog (lineNeedMorePaperclips s) log
    else Right (incHelpersWith inc helpers, decPaperclipsWith price p)

addToErrorLog :: ErrorLogLine -> [ErrorLogLine] -> [ErrorLogLine]
addToErrorLog new existing = existing ++ [new]

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

researchAdvancedHelper :: Seconds -> Paperclips -> AdvancedHelperPrice -> ResearchProgress -> Duration -> [ErrorLogLine] -> Either [ErrorLogLine] (Paperclips, ResearchProgress)
researchAdvancedHelper s p (AdvancedHelperPrice price) progress duration errs =
  case (price > p, progress) of
    (True, NotResearched) -> Left $ addToErrorLog (ErrorLogLine "Not enough paperclips") errs
    (False, NotResearched) -> Right (decPaperclipsWith price p, startResearch duration)
    (_, ResearchInProgress x) -> Left $ addToErrorLog (ErrorLogLine "Already in progress") errs
    (_, ResearchDone) -> Left $ addToErrorLog (ErrorLogLine "Already done") errs

decPaperclipsWith :: Paperclips -> Paperclips -> Paperclips
decPaperclipsWith price paperclips = paperclips - price

incHelpersWith :: Helpers -> Helpers -> Helpers
incHelpersWith inc helpers = helpers + inc

plantASeed :: Seconds -> TreePrice -> TreeSeeds -> Trees -> [ErrorLogLine] -> Either [ErrorLogLine] (TreeSeeds, Trees)
plantASeed s price seeds trees errs =
  if unTreePrice price > seeds
    then Left $ addToErrorLog (lineNeedMoreSeeds s) errs
    else Right (decSeedsWith price seeds, succ trees)

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

decSeedsWith :: TreePrice -> TreeSeeds -> TreeSeeds
decSeedsWith price seeds = seeds - (unTreePrice price)