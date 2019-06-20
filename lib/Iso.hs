{-# LANGUAGE FlexibleContexts #-}

module Iso where

import           Control.Lens                   ( AnIso
                                                , Each
                                                , each
                                                , over
                                                , withIso
                                                )
import           Data.Tuple.Curry               ( Curry
                                                , uncurryN
                                                )

under1 :: AnIso s t r b -> (t -> s) -> b -> r
under1 i g a = withIso i (\con eli -> con $ g (eli a))

under2 :: AnIso s t r b -> (t -> t -> s) -> b -> b -> r
under2 i g a b = withIso i (\con eli -> con $ g (eli a) (eli b))

{- | Apply a function under an isomorphism.

>>> under3 paperclips (\a b c -> a + b + c) (Paperclips 3) (Paperclips 2) (Paperclips 4)
-- 9
-}

under3 :: AnIso s t r b -> (t -> t -> t -> s) -> b -> b -> b -> r
under3 i g a b c = withIso i (\con eli -> con $ g (eli a) (eli b) (eli c))

-------------------------

{- | Apply a function, g2, to each argument before passing arguments to g1.
Allows a variable number of arguments via tuples.

>>> toEachArg (+) (+1) (1,2)
5

>>> toEachArg (\a b c -> a + b + c) (+1) (1,2,3)
9
-}

toEachArg :: (Curry (t1 -> t2) b1, Each s t1 a b2) => b1 -> (a -> b2) -> s -> t2
toEachArg g1 g2 a = uncurryN g1 (over each g2 a)

{- | Apply a function under an isomorphism. Allows a variable number of arguments via tuples.

This is similar to the underN functions but with the following tradeoff:
underAp require the arguments to be passed via a tuple, but will in return
replace all underN functions.

>>> underAp paperclips (\a b -> a + b) (Paperclips 3,Paperclips 2)
5 :: Paperclips

>>> underAp paperclips (\a b c -> a + b + c) (Paperclips 1, Paperclips 2, Paperclips 3)
6 :: Paperclips
-}

underAp
    :: (Curry (t1 -> s1) b1, Each s2 t1 b b2)
    => AnIso s1 b2 r b
    -> b1
    -> s2
    -> r
underAp i fx a = withIso i (\con eli -> con $ toEachArg fx eli a)

-------------------------

unwrap :: AnIso s r a b -> b -> r
unwrap i a = withIso i (\_ eli -> eli a)

-------------------------

-- Walk down 2 independent isos and apply f
walkDown2
    :: AnIso s t a b -> AnIso s1 t1 a1 b1 -> (t -> t1 -> r) -> b -> b1 -> r
walkDown2 iso1 iso2 f a1 a2 =
    withIso iso1 (\_ eli1 -> withIso iso2 (\_ eli2 -> f (eli1 a1) (eli2 a2)))
