{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Config where

import           Elements

data Config = Config
  { _constants :: Constants
  , _prices :: Prices Integer }

data Constants = Constants
  { _helperInc :: HelperInc (Helpers Integer) }

newtype HelperInc a = HelperInc { unHelperInc :: a} deriving (Functor)

instance Applicative HelperInc where
  pure = HelperInc
  HelperInc f <*> HelperInc a = HelperInc (f a)

data Prices a = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice (Paperclips a)
  , _progPrice :: ProgPrice a
  , _treePrice :: TreePrice a }

newtype AdvancedHelperPrice a = AdvancedHelperPrice { unAdvancedHelperPrice :: a }

newtype HelperPrice a = HelperPrice { unHelperPrice :: Paperclips a }

newtype ProgPrice a = ProgPrice { unProgPrice :: a }

newtype TreePrice a = TreePrice { unTreePrice :: a }

newtype TreeSeedPrice = TreeSeedPrice { unTreeSeedPrice :: Paperclips Integer }