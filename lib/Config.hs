{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens

import           Elements

newtype AdvancedHelperPrice a = AdvancedHelperPrice { unAdvancedHelperPrice :: a }

data Prices a = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice (Paperclips a) }

data Config a = Config
  { _constants :: Constants a
  , _prices :: Prices a }

newtype HelperInc a = HelperInc { unHelperInc :: a} deriving (Functor)

data Constants a = Constants
  { _helperInc :: HelperInc (Helpers a) }
makeLenses ''Constants

instance Applicative HelperInc where
  pure = HelperInc
  HelperInc f <*> HelperInc a = HelperInc (f a)

newtype HelperPrice a = HelperPrice { unHelperPrice :: Paperclips a }

newtype ProgPrice a = ProgPrice { unProgPrice :: a }

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
