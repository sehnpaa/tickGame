{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module NaturalTransformation where

import Control.Lens (withIso)

import qualified Iso
import Resources

newtype m :~> n = Nat { unNat :: forall a. m a -> n a}

helpersToPaperclips :: Helpers :~> Paperclips
helpersToPaperclips = Nat (\h -> Paperclips $ withIso Iso.helpers (\_ eli -> eli h))
