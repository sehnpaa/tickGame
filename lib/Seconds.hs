{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Seconds where

import           Control.Lens                   ( makeClassy )

newtype Seconds a = Seconds { _unSeconds :: a } deriving (Enum, Eq)
makeClassy ''Seconds

instance Show (Seconds Integer) where
  show (Seconds a) = show a
