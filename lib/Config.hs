{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Config where

import           Control.Lens

import           Elements

data Config a = Config
  { _constants :: Constants a
  , _prices :: Prices a }

data Constants a = Constants
  { _helperInc :: HelperInc (Helpers a) }

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

constants :: Lens' (Config a) (Constants a)
constants f state =
  (\constants' -> state { _constants = constants' }) <$> f (_constants state)

prices :: Lens' (Config a) (Prices a)
prices f state =
  (\prices' -> state { _prices = prices' }) <$> f (_prices state)

advancedHelperPrice :: Lens' (Prices a) (AdvancedHelperPrice (Paperclips a))
advancedHelperPrice f state =
  (\price' -> state { _advancedHelperPrice = price' })
    <$> f (_advancedHelperPrice state)

treePrice :: Lens' (Prices a) (TreePrice a)
treePrice f state =
  (\treePrice' -> state { _treePrice = treePrice' }) <$> f (_treePrice state)

progPrice :: Lens' (Prices a) (ProgPrice a)
progPrice f state =
  (\price' -> state { _progPrice = price' }) <$> f (_progPrice state)
