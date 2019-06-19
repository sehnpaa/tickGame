{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Config where

import Control.Lens (Profunctor, iso)
import Elements

data Config = Config
  { _constants :: Constants
  , _durations :: Durations
  , _prices :: Prices } deriving (Eq, Show)

data Durations = Durations
  { _treeDuration :: TreeDuration } deriving (Eq, Show)

newtype TreeDuration = TreeDuration { unTreeDuration :: Integer } deriving (Eq, Show)

data Constants = Constants
  { _helperInc :: HelperInc (Helpers Integer) } deriving (Eq, Show)

instance Show (HelperInc (Helpers Integer)) where
  show (HelperInc a) = show a

newtype HelperInc a = HelperInc { unHelperInc :: a} deriving (Eq, Functor)

instance Applicative HelperInc where
  pure = HelperInc
  HelperInc f <*> HelperInc a = HelperInc (f a)

data Prices = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice (Paperclips Integer)
  , _progPrice :: ProgPrice Integer
  , _treePrice :: TreePrice Integer
  , _treeSeedPrice :: TreeSeedPrice } deriving (Eq, Show)

newtype AdvancedHelperPrice a = AdvancedHelperPrice { unAdvancedHelperPrice :: a } deriving (Eq)

instance Show (AdvancedHelperPrice (Paperclips Integer)) where
  show (AdvancedHelperPrice a) = show a

newtype HelperPrice a = HelperPrice { unHelperPrice :: Paperclips a } deriving (Eq, Functor)

instance Show (HelperPrice Integer) where
  show (HelperPrice a) = show a

newtype ProgPrice a = ProgPrice { unProgPrice :: a } deriving (Eq, Show)

newtype TreePrice a = TreePrice { unTreePrice :: a } deriving (Eq, Show)

newtype TreeSeedPrice = TreeSeedPrice { unTreeSeedPrice :: Paperclips Integer } deriving (Eq, Show)

isoTreePrice :: (Profunctor p, Functor f) => p (TreePrice a) (f (TreePrice a)) -> p a (f a)
isoTreePrice = iso TreePrice unTreePrice

