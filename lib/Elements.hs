{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elements where

import Control.Lens

data Elements = Elements
  { _paperclips :: Element Paperclips Integer
  , _helpers :: Element Helpers Integer
  , _trees :: Element Trees Integer
  , _treeSeeds :: Element TreeSeeds Integer
  , _water :: Element Water Integer
  , _wood :: Element Wood Integer} deriving (Eq, Show)

data Element f a = Element
  { _cost :: Acquirement (Cost Integer)
  , _count :: f Integer }

-- Not a sum type, either is AcquirePaperclips
data Acquirement cost
  = AcquirePaperclips cost
  | AcquireHelpers cost
  | AcquireTrees cost
  | AcquireTreeSeeds cost
  | AcquireWater
  | AcquireWood

data AcquirePaperclips cost = PaperclipsManually | PaperclipsFromHelpers
data AcquireHelpers cost = HelpersManually cost
data AcquireTrees cost = TreesFromTreeSeeds cost
data AcquireTreeSeeds cost = BuyTreeSeeds cost
data AcquireWater = WaterManually
data AcquireWood = WoodTODO

instance Eq (Element Paperclips a) where
  a == b = _count a == _count b
instance Eq (Element Helpers a) where
  a == b = _count a == _count b
instance Eq (Element Trees a) where
  a == b = _count a == _count b
instance Eq (Element TreeSeeds a) where
  a == b = _count a == _count b
instance Eq (Element Water a) where
  a == b = _count a == _count b
instance Eq (Element Wood a) where
  a == b = _count a == _count b

instance Show (Element Paperclips a) where
  show = show . _count

instance Show (Element Helpers a) where
  show = show . _count

instance Show (Element Trees a) where
  show = show . _count

instance Show (Element TreeSeeds a) where
  show = show . _count

instance Show (Element Water a) where
  show = show . _count

instance Show (Element Wood a) where
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
  show (Growing n) = show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
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
paperclipCost f state = (\c -> state { _paperclipsCost = c}) <$> f (_paperclipsCost state)