{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Control.Lens
import Data.Text (concat, pack)

import qualified Iso as Iso
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
incHelperIncWith = withIso (Iso.helpers . Iso.helperInc) (\_ elim inc -> under (Iso.helpers . Iso.helperInc) (\h -> h + elim inc))

seedWork :: [Prog] -> Trees -> ([Prog], Trees)
seedWork progs ts =
  let ts' = additionalTrees progs
      progs' = filter (not . isGrowingDone) $ map (\x -> case x of
        NotGrowing -> NotGrowing
        Growing 1 -> GrowingDone
        Growing n -> Growing (n-1)
        GrowingDone -> GrowingDone) progs
    in (progs', ts+ts')

isGrowingDone :: Prog -> Bool
isGrowingDone GrowingDone = True
isGrowingDone _ = False

additionalTrees :: [Prog] -> Trees
additionalTrees = Trees . toInteger . length . filter (\x -> case x of Growing 1 -> True; _ -> False)

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
decPaperclipsWith = withIso (Iso.paperclips . Iso.helperPrice) (\_ elim price -> under Iso.paperclips (\p -> p - elim price))

decPaperclipsWith' :: AdvancedHelperPrice -> Paperclips -> Paperclips
decPaperclipsWith' = withIso (Iso.paperclips . Iso.advancedHelperPrice) (\_ elim price -> under Iso.paperclips (\p -> p - elim price))

plantASeed :: Seconds -> TreeDuration -> TreePrice -> TreeSeeds -> Either ErrorLogLine TreeSeeds
plantASeed s dur price seeds =
  if unTreePrice price > countNotGrowingSeeds seeds
    then Left $ lineNeedMoreSeeds s
    else Right $ initializeSeed dur seeds

initializeSeed :: TreeDuration -> TreeSeeds -> TreeSeeds
initializeSeed duration = TreeSeeds . changeFirst (== NotGrowing) (const $ Growing $ unTreeDuration duration) . unTreeSeeds

changeFirst :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
changeFirst _ _ [] = []
changeFirst f g (x:xs) = if f x then g x : xs else x : changeFirst f g xs

countNotGrowingSeeds :: TreeSeeds -> Integer
countNotGrowingSeeds = toInteger . length . filter isNotGrowing . unTreeSeeds

isNotGrowing :: Prog -> Bool
isNotGrowing a = case a of
  NotGrowing -> True
  _ -> False

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

createPaperclip :: Paperclips -> Paperclips
createPaperclip = under Iso.paperclips succ
