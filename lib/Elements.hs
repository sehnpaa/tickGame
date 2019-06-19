{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elements where

data Elements = Elements
  { _paperclips :: Paperclips Integer
  , _helpers :: Helpers Integer
  , _trees :: Trees Integer
  , _treeSeeds :: TreeSeeds Integer
  , _water :: Water Integer
  , _wood :: Wood Integer } deriving (Eq, Show)

data Element f a = Element
  { _cost :: Cost
  , _count :: f Integer }

data Cost = Cost
  { _paperclipsCost :: Paperclips Integer
  , _helpersCost :: Helpers Integer
  , _treesCost :: Trees Integer
  , _treeSeedsCost :: TreeSeeds Integer
  , _waterCost :: Water Integer
  , _woodCost :: Wood Integer} deriving (Eq, Show)

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

