{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Seconds where

newtype Seconds a = Seconds { unSeconds :: a } deriving (Enum, Eq)

instance Show (Seconds Integer) where
  show (Seconds a) = show a
