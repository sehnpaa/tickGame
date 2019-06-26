{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Elements where

import           Control.Lens

data Duration a = Instant | Ticks a

newtype DurationPaperclips a = DurationPaperclips { unDurationPaperclips :: Duration a }
newtype DurationHelpers a = DurationHelpers { unDurationHelpers :: Duration a }
newtype DurationTrees a = DurationTrees { unDurationTrees :: Duration a }
newtype DurationTreeSeeds a = DurationTreeSeeds { _durationTreeSeeds :: Duration a }
makeLenses ''DurationTreeSeeds

newtype DurationWater a = DurationWater { unDurationWater :: Duration a }
newtype DurationWood a = DurationWood { unDurationWood :: Duration a }

data AcquirePaperclips cost = AcquirePaperclips
  { _paperclipsManually :: PaperclipsManually cost
  , _paperclipsFromHelpers :: PaperclipsFromHelper cost }

newtype PaperclipsManually cost = PaperclipsManually { unPaperclipsManually :: cost }
newtype PaperclipsFromHelper cost = PaperclipsFromHelper { unPaperclipsFromHelper :: cost }

data AcquireHelpers cost = AcquireHelpers
  { _helpersManually :: HelpersManually cost }

newtype HelpersManually cost = HelpersManually { unHelpersManually :: cost }

newtype TreesFromTreeSeeds cost = TreesFromTreeSeeds { unTreesFromTreeSeeds :: cost }
newtype TreeSeedCostPerTick cost = TreeSeedCostPerTick { unTreeSeedCostPerTick :: cost }

data AcquireTrees cost = AcquireTrees
  { _treesFromTreeSeeds :: TreesFromTreeSeeds cost
  , _treeSeedCostPerTick :: TreeSeedCostPerTick cost }
makeLenses ''AcquireTrees

newtype AcquireTreeSeeds cost = AcquireTreeSeeds { unAcquireTreeSeeds :: BuyTreeSeeds cost }
newtype BuyTreeSeeds cost = BuyTreeSeeds { unBuyTreeSeeds :: cost }

newtype AcquireWater cost = AcquireWater { unAquireWater :: WaterManually cost }
newtype WaterManually cost = WaterManually { unWaterManually :: cost }

newtype AcquireWood cost = AcquireWood { unAcquireWood :: WoodManually cost }
newtype WoodManually cost = WoodManually { unWoodManually :: cost }

newtype Paperclips a = Paperclips { unPaperclips :: a } deriving (Enum, Eq, Functor, Ord)

instance Applicative Paperclips where
  pure = Paperclips
  Paperclips f <*> Paperclips a = Paperclips (f a)

instance Show (Paperclips Integer) where
  show (Paperclips a) = show a

newtype Helpers a = Helpers { unHelpers :: a } deriving (Enum, Eq, Functor, Ord)

instance Applicative Helpers where
  pure = Helpers
  Helpers f <*> Helpers a = Helpers (f a)

instance Show (Helpers Integer) where
  show (Helpers a) = show a

newtype Trees a = Trees { unTrees :: a } deriving (Enum, Eq, Num, Ord)

instance Show (Trees Integer) where
  show (Trees a) = show a

newtype TreeSeeds a = TreeSeeds { unTreeSeeds :: [Prog a]} deriving (Eq)

instance Show (TreeSeeds Integer) where
  show (TreeSeeds a) = show a

data Prog a = NotGrowing | Growing a | GrowingDone deriving (Eq)

instance Show (Prog Integer) where
  show NotGrowing = show "Not growing"
  show (Growing n) =
    show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
   where
    noun 1 = "tick"
    noun _ = "ticks"
  show GrowingDone = show "Growing done"

newtype Water a = Water { unWater :: a } deriving (Eq, Functor, Ord)

instance Show (Water Integer) where
  show (Water a) = show a

newtype Wood a = Wood { unWood :: a } deriving (Enum, Eq, Ord)

instance Show (Wood Integer) where
  show (Wood a) = show a

data Cost a = Cost
  { _paperclipCost :: Paperclips a
  , _helpersCost :: Helpers a
  , _treesCost :: Trees a
  , _treeSeedsCost :: TreeSeeds a
  , _waterCost :: Water a
  , _woodCost :: Wood a }
makeLenses ''Cost

data Element acquire duration f a = Element
  { _cost :: acquire (Cost a)
  , _count :: f a
  , _duration :: duration a }
makeLenses ''Element

data Elements a = Elements
  { _paperclips :: Element AcquirePaperclips DurationPaperclips Paperclips a
  , _helpers :: Element AcquireHelpers DurationHelpers Helpers a
  , _trees :: Element AcquireTrees DurationTrees Trees a
  , _treeSeeds :: Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a
  , _water :: Element AcquireWater DurationWater Water a
  , _wood :: Element AcquireWood DurationWood Wood a }
makeLenses ''Elements

helpersManually :: Lens' (AcquireHelpers (Cost a)) (HelpersManually (Cost a))
helpersManually f state =
  (\a -> state { _helpersManually = a }) <$> f (_helpersManually state)

buyTreeSeeds :: Lens' (AcquireTreeSeeds (Cost a)) (BuyTreeSeeds (Cost a))
buyTreeSeeds f state = AcquireTreeSeeds <$> f (unAcquireTreeSeeds state)

elementPaperclips
  :: Lens'
       (Elements a)
       (Element AcquirePaperclips DurationPaperclips Paperclips a)
elementPaperclips f state =
  (\paperclips' -> state { _paperclips = paperclips' })
    <$> f (_paperclips state)

elementHelpers
  :: Lens' (Elements a) (Element AcquireHelpers DurationHelpers Helpers a)
elementHelpers f state =
  (\helpers' -> state { _helpers = helpers' }) <$> f (_helpers state)

elementWater :: Lens' (Elements a) (Element AcquireWater DurationWater Water a)
elementWater f state =
  (\water' -> state { _water = water' }) <$> f (_water state)

elementTrees :: Lens' (Elements a) (Element AcquireTrees DurationTrees Trees a)
elementTrees f state =
  (\trees' -> state { _trees = trees' }) <$> f (_trees state)

elementTreeSeeds
  :: Lens' (Elements a) (Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a)
elementTreeSeeds f state =
  (\treeSeeds' -> state { _treeSeeds = treeSeeds' }) <$> f (_treeSeeds state)

progs :: Lens' (TreeSeeds a) [Prog a]
progs f state = TreeSeeds <$> f (unTreeSeeds state)

elementWood :: Lens' (Elements a) (Element AcquireWood DurationWood Wood a)
elementWood f state = (\wood' -> state { _wood = wood' }) <$> f (_wood state)
