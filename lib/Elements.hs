{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Elements where

import           Control.Lens
import           Control.Applicative            ( liftA2 )
import qualified Data.Text                     as T

data Duration a = Instant | Ticks a

newtype DurationPaperclips a = DurationPaperclips { unDurationPaperclips :: Duration a }
newtype DurationEnergy a = DurationEnergy { unDurationEnergy :: Duration a }
newtype DurationHelpers a = DurationHelpers { unDurationHelpers :: Duration a }
newtype DurationStorage a = DurationStorage { unDurationStorage :: Duration a }
newtype DurationTrees a = DurationTrees { unDurationTrees :: Duration a }
newtype DurationTreeSeeds a = DurationTreeSeeds { _durationTreeSeeds :: Duration a }
makeLenses ''DurationTreeSeeds

newtype DurationWater a = DurationWater { unDurationWater :: Duration a }
newtype DurationWood a = DurationWood { unDurationWood :: Duration a }

data Paperclips a = Paperclips { _paperclips :: a } deriving (Eq, Functor, Ord)
makeLenses ''Paperclips

instance Applicative Paperclips where
  pure = Paperclips
  Paperclips f <*> Paperclips a = Paperclips (f a)

instance Show (Paperclips Integer) where
  show (Paperclips a) = show a

newtype Energy a = Energy { _energy :: a } deriving (Eq, Functor, Ord)
makeLenses ''Energy

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

newtype Storage a = Storage { unStorage :: a } deriving (Functor)

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

newtype TreeSeeds a = TreeSeeds { _treeSeeds :: [Prog a]} deriving (Eq)
makeLenses ''TreeSeeds

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
makeLenses ''Cost

data NoCost a = NoCost

data CostEnergyPaperclips a = CostEnergyPaperclips (Energy a) (Paperclips a)
data CostPaperclips a = CostPaperclips { _costPaperclips :: Paperclips a }
makeLenses ''CostPaperclips
data CostWood a = CostWood { _costWood :: Wood a }
makeLenses ''CostWood
data CostWater a = CostWater { _costWater :: Water a }
makeLenses ''CostWater

data CostTreeSeeds a = CostTreeSeeds (TreeSeeds [Prog a])

newtype PaperclipsManually a = PaperclipsManually { unPaperclipsManually :: NoCost a }
newtype PaperclipsFromHelper a = PaperclipsFromHelper { unPaperclipsFromHelper :: NoCost a }

data AcquirePaperclips a = AcquirePaperclips
  { _paperclipsManually :: PaperclipsManually a
  , _paperclipsFromHelpers :: PaperclipsFromHelper a }
makeLenses ''AcquirePaperclips

data BuyTreeSeeds a = BuyTreeSeeds { unBuyTreeSeeds :: CostPaperclips a, buyTreeSeedsErrorMessage :: T.Text }

newtype AcquireTreeSeeds a = AcquireTreeSeeds { _acquireBuyTreeSeeds :: BuyTreeSeeds a }
makeLenses ''AcquireTreeSeeds

newtype TreesFromTreeSeeds a = TreesFromTreeSeeds { unTreesFromTreeSeeds :: CostTreeSeeds a }
data TreeSeedCostPerTick a = TreeSeedCostPerTick { unTreeSeedCostPerTick :: CostWater a, treeSeedCostPerTickErrorMessage :: T.Text }

data EnergyManually a = EnergyManually { _energyManually :: NoCost a}

data AcquireEnergy cost = AcquireEnergy
  { _acquireEnergyManually :: EnergyManually cost }
makeLenses ''AcquireEnergy

data HelpersManually a = HelpersManually { _helpersManually :: CostEnergyPaperclips a
, _helpersManuallyEnergyErrorMessage :: T.Text
, _helpersManuallyPaperclipsErrorMessage :: T.Text }
makeLenses ''HelpersManually

data AcquireHelpers a = AcquireHelpers
  { _acquireHelpersManually :: HelpersManually a }
makeLenses ''AcquireHelpers

data StorageManually a = StorageManually { _storageManually :: CostWood a, _storageManuallyErrorMessage :: T.Text }
makeLenses ''StorageManually

data AcquireStorage a = AcquireStorage
  { _acquireStorageManually :: StorageManually a }
makeLenses ''AcquireStorage

data AcquireTrees a = AcquireTrees
  { _acquireTreesFromTreeSeeds :: TreesFromTreeSeeds a
  , _acquireTreeSeedCostPerTick :: TreeSeedCostPerTick a }
makeLenses ''AcquireTrees

newtype WaterManually a = WaterManually { unWaterManually :: NoCost a }
newtype AcquireWater a = AcquireWater { _acquireWater :: WaterManually a }

newtype WoodManually a = WoodManually { unWoodManually :: NoCost a }
newtype AcquireWood a = AcquireWood { _acquireWood :: WoodManually a }

data Element acquire duration f a = Element
  { _cost :: acquire a
  , _count :: f a
  , _duration :: duration a }
makeLenses ''Element

data Elements a = Elements
  { _elementsPaperclips :: Element AcquirePaperclips DurationPaperclips Paperclips a
  , _elementEnergy :: Element AcquireEnergy DurationEnergy Energy a
  , _helpers :: Element AcquireHelpers DurationHelpers Helpers a
  , _elementsStorage :: Element AcquireStorage DurationStorage Storage a
  , _elementTrees :: Element AcquireTrees DurationTrees Trees a
  , _elementsTreeSeeds :: Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a
  , _water :: Element AcquireWater DurationWater Water a
  , _wood :: Element AcquireWood DurationWood Wood a }
makeLenses ''Elements

elementPaperclips
  :: Lens'
       (Elements a)
       (Element AcquirePaperclips DurationPaperclips Paperclips a)
elementPaperclips f state =
  (\paperclips' -> state { _elementsPaperclips = paperclips' })
    <$> f (_elementsPaperclips state)

elementHelpers
  :: Lens' (Elements a) (Element AcquireHelpers DurationHelpers Helpers a)
elementHelpers f state =
  (\helpers' -> state { _helpers = helpers' }) <$> f (_helpers state)

elementStorage
  :: Lens' (Elements a) (Element AcquireStorage DurationStorage Storage a)
elementStorage f state = (\storage' -> state { _elementsStorage = storage' })
  <$> f (_elementsStorage state)

elementWater :: Lens' (Elements a) (Element AcquireWater DurationWater Water a)
elementWater f state =
  (\water' -> state { _water = water' }) <$> f (_water state)

elementTreeSeeds
  :: Lens' (Elements a) (Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a)
elementTreeSeeds f state =
  (\treeSeeds' -> state { _elementsTreeSeeds = treeSeeds' })
    <$> f (_elementsTreeSeeds state)

progs :: Lens' (TreeSeeds a) [Prog a]
progs f state = TreeSeeds <$> f (view treeSeeds state)

elementWood :: Lens' (Elements a) (Element AcquireWood DurationWood Wood a)
elementWood f state = (\wood' -> state { _wood = wood' }) <$> f (_wood state)

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
