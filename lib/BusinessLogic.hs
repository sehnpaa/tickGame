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

plantASeed :: Seconds -> TreePrice -> TreeSeeds -> Trees -> [ErrorLogLine] -> Either [ErrorLogLine] (TreeSeeds, Trees)
plantASeed s price seeds trees errs =
  if unTreePrice price > seeds
    then Left $ addToErrorLog (lineNeedMoreSeeds s) errs
    else Right (decSeedsWith price seeds, succ trees)

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

decSeedsWith :: TreePrice -> TreeSeeds -> TreeSeeds
decSeedsWith price seeds = seeds - (unTreePrice price)