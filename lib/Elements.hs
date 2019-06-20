{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elements where

import           Control.Lens

data Elements = Elements
  { _paperclips :: Element AcquirePaperclips Paperclips Integer
  , _helpers :: Element AcquireHelpers Helpers Integer
  , _trees :: Element AcquireTrees Trees Integer
  , _treeSeeds :: Element AcquireTreeSeeds TreeSeeds Integer
  , _water :: Element AcquireWater Water Integer
  , _wood :: Element AcquireWood Wood Integer} deriving (Eq, Show)

data Element acquire f a = Element
  { _cost :: acquire (Cost Integer)
  , _count :: f Integer }

data AcquirePaperclips cost = AcquirePaperclips
  { _paperclipsManually :: PaperclipsManually cost
  , _paperclipsFromHelpers :: PaperclipsFromHelper cost }

newtype PaperclipsManually cost = PaperclipsManually { unPaperclipsManually :: cost }
newtype PaperclipsFromHelper cost = PaperclipsFromHelper { unPaperclipsFromHelper :: cost }

data AcquireHelpers cost = AcquireHelpers
  { _helpersManually :: HelpersManually cost }

newtype HelpersManually cost = HelpersManually { unHelpersManually :: cost }

newtype AcquireTrees cost = AcquireTrees { unAcquireTrees :: TreesFromTreeSeeds cost }
newtype TreesFromTreeSeeds cost = TreesFromTreeSeeds { unTreesFromTreeSeeds :: cost }

newtype AcquireTreeSeeds cost = AcquireTreeSeeds { unAcquireTreeSeeds :: BuyTreeSeeds cost }
newtype BuyTreeSeeds cost = BuyTreeSeeds { unBuyTreeSeeds :: cost }

newtype AcquireWater cost = AcquireWater { unAquireWater :: WaterManually cost }
newtype WaterManually cost = WaterManually { unWaterManually :: cost }

newtype AcquireWood cost = AcquireWood { unAcquireWood :: WoodManually cost }
newtype WoodManually cost = WoodManually { unWoodManually :: cost }


instance Eq (Element ac Paperclips a) where
  a == b = _count a == _count b
instance Eq (Element ac Helpers a) where
  a == b = _count a == _count b
instance Eq (Element ac Trees a) where
  a == b = _count a == _count b
instance Eq (Element ac TreeSeeds a) where
  a == b = _count a == _count b
instance Eq (Element ac Water a) where
  a == b = _count a == _count b
instance Eq (Element ac Wood a) where
  a == b = _count a == _count b

instance Show (Element ac Paperclips a) where
  show = show . _count

instance Show (Element ac Helpers a) where
  show = show . _count

instance Show (Element ac Trees a) where
  show = show . _count

instance Show (Element ac TreeSeeds a) where
  show = show . _count

instance Show (Element ac Water a) where
  show = show . _count

instance Show (Element ac Wood a) where
  show = show . _count

data Cost a = Cost
  { _paperclipsCost :: Paperclips a
  , _helpersCost :: Helpers a
  , _treesCost :: Trees a
  , _treeSeedsCost :: TreeSeeds a
  , _waterCost :: Water a
  , _woodCost :: Wood a } deriving (Eq)

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

paperclipCost :: Lens' (Cost a) (Paperclips a)
paperclipCost f state =
  (\c -> state { _paperclipsCost = c }) <$> f (_paperclipsCost state)

treeSeedCost :: Lens' (Cost a) (TreeSeeds a)
treeSeedCost f state =
  (\c -> state { _treeSeedsCost = c }) <$> f (_treeSeedsCost state)
