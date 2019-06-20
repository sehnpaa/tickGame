{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Config
import Elements
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

seedWork :: (Num a, Ord a) => Seconds -> Water a -> ProgPrice a -> [Prog a] -> Trees a -> Either (ErrorLogLine, [Prog a]) (Water a, [Prog a], Trees a)
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

buyHelper :: (Enum a, Num a, Ord a) => Seconds -> HelpersManually (Cost a) -> Paperclips a -> Helpers a -> Either ErrorLogLine (Helpers a, Paperclips a)
buyHelper s cost p h =
  if needMorePaperclips (unHelpersManually cost) p
    then Left (lineNeedMorePaperclips s)
    else Right (succ h, decPaperclipsWith (unHelpersManually cost) p)

pumpWater :: (Enum a, Num a, Ord a) => Water a -> WaterTank a -> Water a
pumpWater w tank = Water $ min (unWaterTank tank) (succ $ unWater w)

researchAdvancedHelper :: Seconds -> Paperclips Integer -> AdvancedHelperPrice (Paperclips Integer) -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips Integer, ResearchProgress)
researchAdvancedHelper s p price progress duration =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s "Not enough paperclips."
    (False, NotResearched) -> Right (decPaperclipsWith' price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s "Already in progress."
    (_, ResearchDone) -> Left $ mkErrorLogLine s "Already done."

plantASeed :: (Num a, Ord a) => Seconds -> TreeDuration -> TreePrice a -> TreeSeeds a -> Either ErrorLogLine (TreeSeeds a)
plantASeed s dur price seeds =
  if unTreePrice price > countNotGrowingSeeds seeds
    then Left $ lineNeedMoreSeeds s
    else Right $ initializeSeed dur seeds

buyASeed :: (Num a, Ord a) => Seconds -> BuyTreeSeeds (Cost a) -> Paperclips a -> TreeSeeds a -> Either ErrorLogLine (TreeSeeds a, Paperclips a)
buyASeed s cost p (TreeSeeds seeds) =
  if needMorePaperclips' cost p
    then Left $ mkErrorLogLine s "Not enough paperclips."
    else Right $ (TreeSeeds $ seeds ++ [NotGrowing], Paperclips $ (unPaperclips p) - (unPaperclips $ _paperclipsCost $ unBuyTreeSeeds cost))

createPaperclip :: (Enum a, Ord a) => Paperclips a -> Storage (Paperclips a) -> Paperclips a
createPaperclip p storage = min (unStorage storage) $ fmap succ p