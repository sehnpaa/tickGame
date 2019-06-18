{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Elements where

data Elements = Elements
  { _paperclips :: Paperclips Integer
  , _helpers :: Helpers Integer
  , _trees :: Trees
  , _treeSeeds :: TreeSeeds
  , _water :: Water Integer
  , _wood :: Wood } deriving (Eq, Show)

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

newtype Water a = Water { unWater :: a } deriving (Eq, Functor, Ord)

instance Show (Water Integer) where
  show (Water a) = show a

newtype Wood = Wood { unWood :: Integer } deriving (Enum, Eq, Ord)

instance Show Wood where
  show (Wood a) = show a

