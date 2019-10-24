{-# LANGUAGE RankNTypes #-}

module BusinessLogic where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( view )
import           Control.Monad.Reader
import qualified Data.Text                     as T

import           Config
import           Elements
import           Resources
import           Seconds
import           Source
import           State

helperWork
  :: ( Num a
     , Ord a
     , HasHelperInc s a
     , HasHelpers s a
     , HasPaperclips s a
     , HasStorageOfPaperclips s a
     , MonadReader s m
     )
  => m (Paperclips a)
helperWork = do
  p        <- ask $ view paperclips
  h        <- ask $ view helpers
  inc      <- ask $ view helperInc
  storage' <- ask $ view storageOfPaperclips
  return
    $   limitByStorage storage'
    $   (+)
    <$> (Paperclips $ view unHelpers $ (*) <$> h <*> (Helpers . unHelperInc) inc
        )
    <*> p

researchWork
  :: (Eq a, Num a, HasHelperInc s a, HasResearchProgress s a, MonadReader s m)
  => m (ResearchProgress a, HelperInc a)
researchWork = do
  progress <- view $ ask researchProgress
  inc      <- view $ ask helperInc
  return $ case progress of
    NotResearched        -> (progress, inc)
    ResearchInProgress 1 -> (ResearchDone, fmap (* 2) inc)
    ResearchInProgress n -> (ResearchInProgress (n - 1), inc)
    ResearchDone         -> (progress, inc)

seedWork
  :: ( Eq a
     , Ord a
     , Num a
     , Show a
     , HasSeconds s a
     , HasTreeSeedCostPerTick s a
     , HasTreeSeeds s a
     , HasTrees s a
     , HasWater s a
     , MonadReader s m
     )
  => m (Either (ErrorLogLine, [Prog a]) (Water a, [Prog a], Trees a))
seedWork = do
  s  <- ask $ view seconds
  w  <- ask $ view water
  (TreeSeedCostPerTick price errorMessage) <- ask $ view treeSeedCostPerTick
  (TreeSeeds ps                          ) <- ask $ view treeSeeds
  ts <- ask $ view trees
  let ts'    = additionalTrees ps
  let progs' = filter (not . isGrowingDone) $ progressGrowing ps
  return $ if any isGrowing ps
    then case calcRemainingWater price ps w of
      Nothing -> Left (mkErrorLogLine s errorMessage, removeGrowingSeeds ps)
      Just w' -> Right $ (w', progs', ts + ts')
    else Right $ (w, ps, ts)

buyHelper
  :: ( Enum a
     , Num a
     , Ord a
     , Show a
     , HasCostEnergyPaperclips s a
     , HasEnergy s a
     , HasEnergyErrorMessage s
     , HasHelpers s a
     , HasPaperclips s a
     , HasPaperclipsErrorMessage s
     , HasSeconds s a
     , MonadReader s m
     )
  => m (Either ErrorLogLine (Helpers a, Energy a, Paperclips a))
buyHelper = do
  s  <- ask $ view seconds
  ce <- ask $ view costEnergyPaperclipsE
  cp <- ask $ view costEnergyPaperclipsP
  (EnergyErrorMessage     energyErr    ) <- ask $ view energyErrorMessage
  (PaperclipsErrorMessage paperclipsErr) <- ask $ view paperclipsErrorMessage
  p  <- ask $ view paperclips
  e  <- ask $ view energy
  h  <- ask $ view helpers
  return $ case (ce <= e, cp <= p) of
    (True , True ) -> Right (succ h, liftA2 (-) e ce, liftA2 (-) p cp)
    (True , False) -> Left $ mkErrorLogLine s paperclipsErr
    (False, True ) -> Left $ mkErrorLogLine s energyErr
    (False, False) ->
      Left $ mkErrorLogLine s $ energyErr <> T.pack " " <> paperclipsErr

pumpWater
  :: (Enum a, Num a, Ord a, HasWater s a, HasWaterTank s a, MonadReader s m)
  => m (Water a)
pumpWater = do
  w    <- ask $ view water
  tank <- ask $ view waterTank
  return $ Water $ min (unWaterTank tank) (succ $ unWater w)

researchAdvancedHelper
  :: ( Num a
     , Ord a
     , Show a
     , MonadReader s m
     , HasSeconds s a
     , HasPaperclips s a
     , HasResearchComp s a
     , HasAdvancedHelperPriceInPaperclips s a
     )
  => m (Either ErrorLogLine (Paperclips a, ResearchProgress a))
researchAdvancedHelper = do
  s     <- ask $ view seconds
  p     <- ask $ view paperclips
  price <- ask $ view advancedHelperPriceInPaperclips
  (ResearchComp dur progress errorMessage inProgressErr doneErr) <- ask
    $ view researchComp
  return
    $ case
        ((Paperclips . unAdvancedHelperPriceInPaperclips $ price) > p, progress)
      of
        (True, NotResearched) -> Left $ mkErrorLogLine s errorMessage
        (False, NotResearched) ->
          Right (decPaperclipsWith' price p, startResearch dur)
        (_, ResearchInProgress _) -> Left $ mkErrorLogLine s inProgressErr
        (_, ResearchDone        ) -> Left $ mkErrorLogLine s doneErr

plantASeed
  :: ( Num a
     , Ord a
     , Show a
     , HasSeconds s a
     , HasDurationTreeSeeds s a
     , HasTreeSeeds s a
     , MonadReader s m
     )
  => m (Either ErrorLogLine (TreeSeeds a))
plantASeed = do
  s     <- ask $ view seconds
  dur   <- ask $ view durationTreeSeeds
  seeds <- ask $ view treeSeeds
  return $ if countNotGrowingSeeds seeds > 0
    then Right $ initializeASeed dur seeds
    else Left $ lineNeedMoreSeeds s

buyASeed
  :: ( Num a
     , Ord a
     , Show a
     , HasTreeSeeds s a
     , HasPaperclips s a
     , HasBuyTreeSeeds s a
     , HasSeconds s a
     , MonadReader s m
     )
  => m (Either ErrorLogLine (TreeSeeds a, Paperclips a))
buyASeed = do
  s                             <- ask $ view seconds
  (BuyTreeSeeds c errorMessage) <- ask $ view buyTreeSeeds
  p                             <- ask $ view paperclips
  (TreeSeeds seeds)             <- ask $ view treeSeeds
  return $ case calcPaperclips c p of
    Nothing -> Left $ mkErrorLogLine s errorMessage
    Just p' -> Right $ (TreeSeeds $ seeds ++ [NotGrowing], p')

generateEnergy
  :: (Enum a, Num a, Ord a, Show a, HasEnergy s a, MonadReader s m)
  => m (Energy a)
generateEnergy = fmap succ (ask $ view energy)

createPaperclip
  :: ( HasPaperclips s a
     , HasStorageOfPaperclips s a
     , MonadReader s m
     , Ord a
     , Enum a
     )
  => m (Paperclips a)
createPaperclip = do
  p        <- ask $ view paperclips
  storage' <- ask $ view storageOfPaperclips
  return $ limitByStorage storage' (fmap succ p)

extendStorage
  :: ( Num a
     , Ord a
     , Show a
     , MonadReader s m
     , HasSeconds s a
     , HasStorageManually s a
     , HasStorageOfPaperclips s a
     , HasWood s a
     )
  => m (Either ErrorLogLine (StorageOfPaperclips a, Wood a))
extendStorage = do
  sec      <- ask $ view seconds
  (StorageManually (CostWood price) errorMessage) <- ask $ view storageManually
  w        <- ask $ view wood
  storage' <- ask $ view storageOfPaperclips
  return $ if w >= price
    then Right (fmap (+ 1) storage', (-) <$> w <*> price)
    else Left $ mkErrorLogLine sec errorMessage

run
  :: ( Integral a
     , HasPaperclips s a
     , HasSeconds s a
     , HasSource s a
     , HasStorageOfPaperclips s a
     , MonadReader s m
     )
  => m (Paperclips a)
run = do
  s              <- ask $ view seconds
  p              <- ask $ view paperclips
  (SourceText t) <- ask $ view sourceText
  storage'       <- ask $ view storageOfPaperclips
  return $ case parse t of
    Left _ -> p
    Right (SyncPaperclipsWithSeconds s') ->
      if view unSeconds s == s' then Paperclips $ view unSeconds s else p
    Right (AddPaperclips ss) ->
      if elem s ss then limitByStorage storage' (fmap (+ 10) p) else p
