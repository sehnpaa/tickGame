{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import           Config
import           Elements
import           Resources
import           Seconds
import           Source
import           State
import           Utils

helperWork
  :: (Num a, Ord a)
  => Paperclips a
  -> Helpers a
  -> HelperInc (Helpers a)
  -> Storage (Paperclips a)
  -> Paperclips a
helperWork p h inc s = limitByStorage s $ addHelperWork inc h p

researchWork
  :: (Eq a, Num a)
  => ResearchProgress a
  -> HelperInc (Helpers a)
  -> (ResearchProgress a, HelperInc (Helpers a))
researchWork progress c = case progress of
  NotResearched        -> (progress, c)
  ResearchInProgress 1 -> (ResearchDone, twoLevels (+) c c)
  ResearchInProgress n -> (ResearchInProgress (n - 1), c)
  ResearchDone         -> (progress, c)

seedWork
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> Water a
  -> TreeSeedCostPerTick a
  -> [Prog a]
  -> Trees a
  -> Either
       (ErrorLogLine, [Prog a])
       (Water a, [Prog a], Trees a)
seedWork s w price ps ts =
  let ts'    = additionalTrees ps
      progs' = filter (not . isGrowingDone) $ progressGrowing ps
  in  if any isGrowing ps
        then case calcRemainingWater price ps w of
          Nothing ->
            Left
              ( mkErrorLogLine s "Not enough water for the seeds."
              , removeGrowingSeeds ps
              )
          Just w' -> Right $ (w', progs', ts + ts')
        else Right $ (w, ps, ts)

buyHelper
  :: (Enum a, Num a, Ord a, Show a)
  => Seconds a
  -> HelpersManually a
  -> Paperclips a
  -> Energy a
  -> Helpers a
  -> Either ErrorLogLine (Helpers a, Energy a, Paperclips a)
buyHelper s (HelpersManually c) p e h = case calcEnergyPaperclipsCombo c e p of
  Left  errors   -> Left $ concatErrors s errors
  Right (e', p') -> Right (succ h, e', p')

pumpWater :: (Enum a, Num a, Ord a) => Water a -> WaterTank a -> Water a
pumpWater w tank = Water $ min (unWaterTank tank) (succ $ unWater w)

researchAdvancedHelper
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> Paperclips a
  -> AdvancedHelperPrice (Paperclips a)
  -> ResearchProgress a
  -> DurationAdvancedHelper a
  -> Either ErrorLogLine (Paperclips a, ResearchProgress a)
researchAdvancedHelper s p price progress dur =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s "Not enough paperclips."
    (False, NotResearched) ->
      Right (decPaperclipsWith' price p, startResearch dur)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s "Already in progress."
    (_, ResearchDone        ) -> Left $ mkErrorLogLine s "Already done."

plantASeed
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> DurationTreeSeeds a
  -> TreeSeeds a
  -> Either ErrorLogLine (TreeSeeds a)
plantASeed s dur seeds = if countNotGrowingSeeds seeds > 0
  then Right $ initializeASeed dur seeds
  else Left $ lineNeedMoreSeeds s

buyASeed
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> BuyTreeSeeds a
  -> Paperclips a
  -> TreeSeeds a
  -> Either ErrorLogLine (TreeSeeds a, Paperclips a)
buyASeed s (BuyTreeSeeds c) p (TreeSeeds seeds) = case calcPaperclips c p of
  Left  err -> Left $ concatErrors s err
  Right p'  -> Right $ (TreeSeeds $ seeds ++ [NotGrowing], p')

generateEnergy
  :: (Enum a, Num a, Ord a, Show a) => EnergyManually a -> Energy a -> Energy a
generateEnergy (EnergyManually c) = freeEnergy c

createPaperclip
  :: (Enum a, Ord a) => Paperclips a -> Storage (Paperclips a) -> Paperclips a
createPaperclip p s = min (unStorage s) $ fmap succ p

extendStorage
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> Wood a
  -> Storage (Paperclips a)
  -> Either ErrorLogLine (Storage (Paperclips a), Wood a)
extendStorage sec (Wood w) s = if w >= 1
  then Right ((fmap . fmap) (+ 1) s, Wood $ w - 1)
  else Left $ mkErrorLogLine sec "Not enough paperclips."

run
  :: (Eq a, Integral a, Num a)
  => Seconds a
  -> Paperclips a
  -> SourceText
  -> Storage (Paperclips a)
  -> Paperclips a
run s p (SourceText t) storage' = case parse t of
  Left _ -> p
  Right (SyncPaperclipsWithSeconds s') ->
    if unSeconds s == s' then Paperclips . unSeconds $ s else p
  Right (AddPaperclips ss) ->
    if elem s ss then limitByStorage storage' (fmap (+ 10) p) else p
