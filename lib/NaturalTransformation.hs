{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module NaturalTransformation where

newtype m :~> n = Nat { unNat :: forall a. m a -> n a}