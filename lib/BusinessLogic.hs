{-# LANGUAGE RankNTypes #-}

module BusinessLogic where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Getter
                                                , view
                                                )
import qualified Data.Text                     as T

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
helperWork p h inc s =
  limitByStorage s
    $   (+)
    <$> (Paperclips $ view unHelpers $ (*) <$> h <*> (unHelperInc inc))
    <*> p

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
  :: ( Enum (helpers a)
     , Ord (paperclips a)
     , Ord (energy a)
     , Num a
     , Applicative energy
     , Applicative paperclips
     , HasSeconds sec a
     , HasEnergy (energy a) a
     , HasPaperclips (paperclips a) a
     , HasHelpers (helpers a) a
     )
  => Getter s sec
  -> Getter s (energy a)
  -> Getter s (paperclips a)
  -> Getter s T.Text
  -> Getter s T.Text
  -> Getter s (paperclips a)
  -> Getter s (energy a)
  -> Getter s (helpers a)
  -> (sec -> T.Text -> errorLogLine)
  -> s
  -> Either
       errorLogLine
       (helpers a, energy a, paperclips a)
buyHelper s ce cp energyErr paperclipsErr p e h f st =
  case ((view ce st) <= (view e st), (view cp st) <= (view p st)) of
    (True, True) -> Right
      ( succ (view h st)
      , liftA2 (-) (view e st) (view ce st)
      , liftA2 (-) (view p st) (view cp st)
      )
    (True , False) -> Left (f (view s st) (view paperclipsErr st))
    (False, True ) -> Left (f (view s st) (view energyErr st))
    (False, False) ->
      Left
        $  f (view s st)
        $  (view energyErr st)
        <> T.pack " "
        <> (view paperclipsErr st)

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
  :: (Enum e, Num a, Ord a, Show a, HasEnergy e a) => Getter s e -> s -> e
generateEnergy e st = succ (view e st)

createPaperclip
  :: ( Functor paperclips
     , Ord (paperclips a)
     , HasPaperclips (paperclips a) a
     , Enum a
     )
  => Getter s (paperclips a)
  -> Getter s (paperclips a)
  -> s
  -> paperclips a
createPaperclip p storage' st =
  (\x y -> min y $ fmap succ x) (view p st) (view storage' st)

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
    -- if view unSeconds s == s' then Paperclips . unSeconds $ _ $ s else p
    if view unSeconds s == s' then Paperclips $ view unSeconds s else p
  Right (AddPaperclips ss) ->
    if elem s ss then limitByStorage storage' (fmap (+ 10) p) else p
