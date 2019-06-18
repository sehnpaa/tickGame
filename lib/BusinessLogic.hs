{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import qualified Iso as Iso
import Config
import Mod
import Resources
import Utils

helperWork :: (Num a, Ord a) => Paperclips a -> Helpers a -> HelperInc (Helpers a) -> Storage (Paperclips a) -> Paperclips a
helperWork p h inc storage = limitByStorage storage $ addHelperWork inc h p

researchWork :: Num a => ResearchProgress -> HelperInc (Helpers a) -> (ResearchProgress, HelperInc (Helpers a))
researchWork progress c =
  case progress of
    NotResearched -> (progress, c)
    ResearchInProgress 1 -> (ResearchDone, twoLevels (+) c c)
    ResearchInProgress n -> (ResearchInProgress (n-1), c)
    ResearchDone -> (progress, c)

seedWork :: Seconds -> Water -> ProgPrice -> [Prog] -> Trees -> Either (ErrorLogLine, [Prog]) (Water, [Prog], Trees)
seedWork s water price progs ts =
  let ts' = additionalTrees progs
      progs' = filter (not . isGrowingDone) $ progressGrowing progs
    in
      if any isGrowing progs
        then
          case calcRemainingWater price progs water of
            Nothing -> Left (mkErrorLogLine s "Not enough water for the seeds.", removeGrowingSeeds progs)
            Just water' -> Right $ (water', progs', ts+ts')
        else Right $ (water, progs, ts)

buyHelper :: (Enum a, Num a, Ord a) => Seconds -> HelperPrice a -> Paperclips a -> Helpers a -> Either ErrorLogLine (Helpers a, Paperclips a)
buyHelper s price p h =
  if unHelperPrice price > p
    then Left (lineNeedMorePaperclips s)
    else Right (succ h, decPaperclipsWith price p)

pumpWater :: Water -> WaterTank -> Water
pumpWater w tank = Water $ min (unWaterTank tank) (succ $ unWater w)

researchAdvancedHelper :: Seconds -> Paperclips Integer -> AdvancedHelperPrice (Paperclips Integer) -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips Integer, ResearchProgress)
researchAdvancedHelper s p price progress duration =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s "Not enough paperclips."
    (False, NotResearched) -> Right (decPaperclipsWith' price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s "Already in progress."
    (_, ResearchDone) -> Left $ mkErrorLogLine s "Already done."

plantASeed :: Seconds -> TreeDuration -> TreePrice -> TreeSeeds -> Either ErrorLogLine TreeSeeds
plantASeed s dur price seeds =
  if unTreePrice price > countNotGrowingSeeds seeds
    then Left $ lineNeedMoreSeeds s
    else Right $ initializeSeed dur seeds

buyASeed :: Seconds -> TreeSeedPrice -> Paperclips Integer -> TreeSeeds -> Either ErrorLogLine (TreeSeeds, Paperclips Integer)
buyASeed s (TreeSeedPrice price) p (TreeSeeds seeds) =
  if price > p
    then Left $ mkErrorLogLine s "Not enough paperclips."
    else Right $ (TreeSeeds $ seeds ++ [NotGrowing], Iso.underAp isoPaperclips (-) (p,price))

createPaperclip :: (Enum a, Ord a) => Paperclips a -> Storage (Paperclips a) -> Paperclips a
createPaperclip p storage = min (unStorage storage) $ fmap succ p