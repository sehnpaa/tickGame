{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resources where

data Resources = Resources
  { _paperclips :: Paperclips Integer
  , _helpers :: Helpers Integer
  , _storage :: Storage (Paperclips Integer)
  , _trees :: Trees
  , _treeSeeds :: TreeSeeds
  , _water :: Water
  , _waterTank :: WaterTank
  , _wood :: Wood } deriving (Eq, Show)

newtype Paperclips a = Paperclips { unPaperclips :: a} deriving (Enum, Eq, Functor, Ord)

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

newtype Storage a = Storage { unStorage :: a } deriving (Eq, Functor)

instance Show (Storage (Paperclips Integer)) where
  show (Storage a) = show a

newtype Trees = Trees { unTrees :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Trees where
  show (Trees a) = show a

newtype TreeSeeds = TreeSeeds { unTreeSeeds :: [Prog]} deriving (Eq)

instance Show TreeSeeds where
  show (TreeSeeds a) = show a

data Prog = NotGrowing | Growing Integer | GrowingDone deriving (Eq)

instance Show Prog where
  show NotGrowing = show "Not growing"
  show (Growing n) = show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
    where
      noun 1 = "tick"
      noun _ = "ticks"
  show GrowingDone = show "Growing done"

newtype Water = Water { unWater :: Integer } deriving (Eq, Ord)

instance Show Water where
  show (Water a) = show a

newtype WaterTank = WaterTank { unWaterTank :: Integer } deriving (Eq)

instance Show WaterTank where
  show (WaterTank a) = show a

newtype Wood = Wood { unWood :: Integer } deriving (Enum, Eq, Ord)

instance Show Wood where
  show (Wood a) = show a

