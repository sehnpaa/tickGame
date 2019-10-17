{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Elements where

import           Control.Lens            hiding ( element
                                                , elements
                                                )
import           Control.Applicative            ( liftA2 )
import qualified Data.Text                     as T

data Duration a = Instant | Ticks a

newtype DurationPaperclips a = DurationPaperclips { unDurationPaperclips :: Duration a }
newtype DurationEnergy a = DurationEnergy { unDurationEnergy :: Duration a }
newtype DurationHelpers a = DurationHelpers { unDurationHelpers :: Duration a }
newtype DurationStorage a = DurationStorage { unDurationStorage :: Duration a }
newtype DurationTrees a = DurationTrees { unDurationTrees :: Duration a }
newtype DurationTreeSeeds a = DurationTreeSeeds { _durationTreeSeedsDur :: Duration a }
makeClassy ''DurationTreeSeeds

newtype DurationWater a = DurationWater { unDurationWater :: Duration a }
newtype DurationWood a = DurationWood { unDurationWood :: Duration a }

newtype Paperclips a = Paperclips { _unPaperclips :: a } deriving (Eq, Functor, Ord)
makeClassy ''Paperclips

instance Applicative Paperclips where
  pure = Paperclips
  Paperclips f <*> Paperclips a = Paperclips (f a)

instance Show (Paperclips Integer) where
  show (Paperclips a) = show a

newtype Energy a = Energy a deriving (Eq, Functor, Ord)

instance Applicative Energy where
  pure = Energy
  Energy f <*> Energy a = Energy (f a)

instance Show (Energy Integer) where
  show (Energy a) = show a

newtype Helpers a = Helpers { unHelpers :: a } deriving (Enum, Eq, Functor, Ord)

instance Applicative Helpers where
  pure = Helpers
  Helpers f <*> Helpers a = Helpers (f a)

instance Show (Helpers Integer) where
  show (Helpers a) = show a

newtype Storage a = Storage { _unStorage :: a } deriving (Functor)
makeClassy ''Storage

instance Show (Storage (Paperclips Integer)) where
  show (Storage a) = show a

newtype Trees a = Trees { unTrees :: a } deriving (Enum, Eq, Num, Ord)

instance Show (Trees Integer) where
  show (Trees a) = show a

data Prog a = NotGrowing | Growing a | GrowingDone deriving (Eq)

instance Show (Prog Integer) where
  show NotGrowing = show "Not growing"
  show (Growing n) =
    show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
   where
    noun 1 = "tick"
    noun _ = "ticks"
  show GrowingDone = show "Growing done"

newtype TreeSeeds a = TreeSeeds { _progs :: [Prog a]} deriving (Eq)
makeClassy ''TreeSeeds

instance Show (TreeSeeds Integer) where
  show (TreeSeeds a) = show a

newtype Water a = Water { unWater :: a } deriving (Eq, Ord)

instance Show (Water Integer) where
  show (Water a) = show a

newtype Wood a = Wood { unWood :: a } deriving (Enum, Eq, Functor, Ord)

instance Applicative Wood where
  pure = Wood
  (Wood f) <*> (Wood a) = Wood $ f a

instance Show (Wood Integer) where
  show (Wood a) = show a

data Cost a = Cost
  { _paperclipCost :: Paperclips a
  , _energyCost :: Energy a
  , _helpersCost :: Helpers a
  , _treesCost :: Trees a
  , _treeSeedsCost :: TreeSeeds a
  , _waterCost :: Water a
  , _woodCost :: Wood a }
makeClassy ''Cost

data NoCost a = NoCost

data CostEnergyPaperclips a = CostEnergyPaperclips (Energy a) (Paperclips a)

-- FIX: Messed up naming
data CostPaperclips a = CostPaperclips { _costPaperclipsA :: Paperclips a }
makeClassy ''CostPaperclips
data CostWood a = CostWood { _costWoodA :: Wood a }
makeClassy ''CostWood
data CostWater a = CostWater { _costWaterA :: Water a }
makeClassy ''CostWater

data CostTreeSeeds a = CostTreeSeeds (TreeSeeds [Prog a])

newtype PaperclipsManually a = PaperclipsManually { unPaperclipsManually :: NoCost a }
newtype PaperclipsFromHelper a = PaperclipsFromHelper { unPaperclipsFromHelper :: NoCost a }

data AcquirePaperclips a = AcquirePaperclips
  { _paperclipsManually :: PaperclipsManually a
  , _paperclipsFromHelpers :: PaperclipsFromHelper a }
makeClassy ''AcquirePaperclips

data BuyTreeSeeds a = BuyTreeSeeds { unBuyTreeSeeds :: CostPaperclips a, buyTreeSeedsErrorMessage :: T.Text }

newtype AcquireTreeSeeds a = AcquireTreeSeeds { _acquireBuyTreeSeeds :: BuyTreeSeeds a }
makeClassy ''AcquireTreeSeeds

newtype TreesFromTreeSeeds a = TreesFromTreeSeeds { unTreesFromTreeSeeds :: CostTreeSeeds a }
data TreeSeedCostPerTick a = TreeSeedCostPerTick { unTreeSeedCostPerTick :: CostWater a, treeSeedCostPerTickErrorMessage :: T.Text }

data EnergyManually a = EnergyManually { _energyManually :: NoCost a}

data AcquireEnergy cost = AcquireEnergy
  { _acquireEnergyManually :: EnergyManually cost }
makeClassy ''AcquireEnergy

data HelpersManually a = HelpersManually { _helpersManuallyCost :: CostEnergyPaperclips a
, _helpersManuallyEnergyErrorMessage :: T.Text
, _helpersManuallyPaperclipsErrorMessage :: T.Text }
makeClassy ''HelpersManually

data AcquireHelpers a = AcquireHelpers
  { _acquireHelpersManually :: HelpersManually a }
makeClassy ''AcquireHelpers

data StorageManually a = StorageManually { _storageManuallyCost :: CostWood a, _storageManuallyErrorMessage :: T.Text }
makeClassy ''StorageManually

data AcquireStorage a = AcquireStorage
  { _acquireStorageManually :: StorageManually a }
makeClassy ''AcquireStorage

data AcquireTrees a = AcquireTrees
  { _acquireTreesFromTreeSeeds :: TreesFromTreeSeeds a
  , _acquireTreeSeedCostPerTick :: TreeSeedCostPerTick a }
makeClassy ''AcquireTrees

newtype WaterManually a = WaterManually { unWaterManually :: NoCost a }
newtype AcquireWater a = AcquireWater { _acquireWater :: WaterManually a }

newtype WoodManually a = WoodManually { unWoodManually :: NoCost a }
newtype AcquireWood a = AcquireWood { _acquireWood :: WoodManually a }

data Element acquire duration f a = Element
  { _elementCost :: acquire a
  , _count :: f a
  , _duration :: duration a }
makeClassy ''Element

data Elements a = Elements
  { _elementsPaperclips :: Element AcquirePaperclips DurationPaperclips Paperclips a
  , _elementsEnergy :: Element AcquireEnergy DurationEnergy Energy a
  , _elementsHelpers :: Element AcquireHelpers DurationHelpers Helpers a
  , _elementsStorage :: Element AcquireStorage DurationStorage Storage a
  , _elementsTrees :: Element AcquireTrees DurationTrees Trees a
  , _elementsTreeSeeds :: Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a
  , _elementsWater :: Element AcquireWater DurationWater Water a
  , _elementsWood :: Element AcquireWood DurationWood Wood a }
makeClassy ''Elements

freeEnergy :: (Enum a) => NoCost a -> Energy a -> Energy a
freeEnergy = const (fmap succ)

calcEnergyPaperclipsCombo
  :: (Num a, Ord a)
  => CostEnergyPaperclips a
  -> Energy a
  -> Paperclips a
  -> T.Text
  -> T.Text
  -> Either T.Text (Energy a, Paperclips a)
calcEnergyPaperclipsCombo (CostEnergyPaperclips ce cp) e p energyErr paperclipsErr
  = case (ce <= e, cp <= p) of
    (True , True ) -> Right (liftA2 (-) e ce, liftA2 (-) p cp)
    (True , False) -> Left paperclipsErr
    (False, True ) -> Left energyErr
    (False, False) -> Left $ energyErr <> T.pack " " <> paperclipsErr

calcPaperclips
  :: (Num a, Ord a) => CostPaperclips a -> Paperclips a -> Maybe (Paperclips a)
calcPaperclips (CostPaperclips c) p =
  if c <= p then Just (liftA2 (-) p c) else Nothing
