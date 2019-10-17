{-# LANGUAGE RankNTypes #-}

module BusinessLogic where

import Control.Lens (Getter, view)

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
seedWork s w (TreeSeedCostPerTick price errorMessage) ps ts =
  let ts'    = additionalTrees ps
      progs' = filter (not . isGrowingDone) $ progressGrowing ps
  in  if any isGrowing ps
        then case calcRemainingWater price ps w of
          Nothing ->
            Left (mkErrorLogLine s errorMessage, removeGrowingSeeds ps)
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
buyHelper s (HelpersManually c energyErr paperclipsErr) p e h =
  case calcEnergyPaperclipsCombo c e p energyErr paperclipsErr of
    Left  errors   -> Left $ mkErrorLogLine s errors
    Right (e', p') -> Right (succ h, e', p')

pumpWater :: (Enum a, Num a, Ord a) => Water a -> WaterTank a -> Water a
pumpWater w tank = Water $ min (unWaterTank tank) (succ $ unWater w)

researchAdvancedHelper
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> Paperclips a
  -> AdvancedHelperPrice (Paperclips a)
  -> ResearchComp a
  -> Either ErrorLogLine (Paperclips a, ResearchProgress a)
researchAdvancedHelper s p price (ResearchComp dur progress errorMessage inProgressErr doneErr)
  = case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s errorMessage
    (False, NotResearched) ->
      Right (decPaperclipsWith' price p, startResearch dur)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s inProgressErr
    (_, ResearchDone        ) -> Left $ mkErrorLogLine s doneErr

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
buyASeed s (BuyTreeSeeds c errorMessage) p (TreeSeeds seeds) =
  case calcPaperclips c p of
    Nothing -> Left $ mkErrorLogLine s errorMessage
    Just p' -> Right $ (TreeSeeds $ seeds ++ [NotGrowing], p')

generateEnergy
  :: (Enum a, Num a, Ord a, Show a) => EnergyManually a -> Energy a -> Energy a
generateEnergy (EnergyManually c) = freeEnergy c

-- createPaperclip
--   :: (Enum a, Ord a) => Paperclips a -> Storage (Paperclips a) -> Paperclips a
-- createPaperclip p s = min (unStorage s) $ (fmap succ p)

createPaperclip :: (Enum a, Ord (f a), Functor f) =>
       Getter s (f a)
       -> Getter s s2
       -> Getter s2 (f a)
       -> s
       -> f a
-- createPaperclip :: (Enum a, Ord a) =>
--        Getter s (Paperclips a)
--        -> Getter s (Storage (Paperclips a))
--        -> Getter (Storage (Paperclips a)) (Paperclips a)
--        -> s
--        -> Paperclips a
createPaperclip l1 l2 l3 s = createPaperclip2 l3 (view l1 s) (view l2 s)

createPaperclip2 :: (Ord (f b), Enum b, Functor f) =>
                      Getter s (f b) -> f b -> s -> f b
createPaperclip2 l3 paperclipCount storageLimit = min (view l3 storageLimit) $ fmap succ paperclipCount


extendStorage
  :: (Num a, Ord a, Show a)
  => Seconds a
  -> StorageManually a
  -> Wood a
  -> Storage (Paperclips a)
  -> Either ErrorLogLine (Storage (Paperclips a), Wood a)
extendStorage sec (StorageManually (CostWood price) errorMessage) w s =
  if w >= price
    then Right ((fmap . fmap) (+ 1) s, (-) <$> w <*> price)
    else Left $ mkErrorLogLine sec errorMessage

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
