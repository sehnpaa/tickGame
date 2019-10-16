{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Control.Lens

import           Elements

newtype AdvancedHelperPrice a = AdvancedHelperPrice { unAdvancedHelperPrice :: a }

data Prices a = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice (Paperclips a) }
makeClassy ''Prices

newtype HelperInc a = HelperInc { unHelperInc :: a} deriving (Functor)

data Constants a = Constants
  { _helperInc :: HelperInc (Helpers a) }
makeClassy ''Constants

data Config a = Config
  { _configConstants :: Constants a
  , _configPrices :: Prices a }
makeClassy ''Config

instance Applicative HelperInc where
  pure = HelperInc
  HelperInc f <*> HelperInc a = HelperInc (f a)

newtype HelperPrice a = HelperPrice { unHelperPrice :: Paperclips a }

newtype ProgPrice a = ProgPrice { unProgPrice :: a }

newtype TreeSeedPrice = TreeSeedPrice { unTreeSeedPrice :: Paperclips Integer }