module LensUtils where

import Control.Lens

getInput1 :: Getting a s a -> s -> a
getInput1 = view

getInput2 :: Getting a s a -> Getting b s b -> s -> (a, b)
getInput2 f1 f2 s = (view f1 s, view f2 s)

getInput3
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> s
  -> (a, b, c)
getInput3 f1 f2 f3 s =
  ( view f1 s
  , view f2 s
  , view f3 s )

getInput4
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> s
  -> (a, b, c, d)
getInput4 f1 f2 f3 f4 s =
  ( view f1 s
  , view f2 s
  , view f3 s
  , view f4 s )

getInput5
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> s
  -> (a, b, c, d, e)
getInput5 f1 f2 f3 f4 f5 s =
  ( view f1 s
  , view f2 s
  , view f3 s
  , view f4 s
  , view f5 s )

getInput6
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> Getting f s f
  -> s
  -> (a, b, c, d, e, f)
getInput6 f1 f2 f3 f4 f5 f6 s =
  ( view f1 s
  , view f2 s
  , view f3 s
  , view f4 s
  , view f5 s
  , view f6 s )

arg1
  :: (a -> b)
  -> Getting a s a
  -> s
  -> b
arg1 f f1 s = f $ getInput1 f1 s

arg2
  :: (a -> b -> c)
  -> Getting a s a
  -> Getting b s b
  -> s
  -> c
arg2 f f1 f2 s = uncurry2 f $ getInput2 f1 f2 s

arg4
  :: (a -> b -> c -> d -> e)
  -> Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> s
  -> e
arg4 f f1 f2 f3 f4 s = uncurry4 f $ getInput4 f1 f2 f3 f4 s

arg5
  :: (a -> b -> c -> d -> e -> f)
  -> Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> s
  -> f
arg5 f f1 f2 f3 f4 f5 s = uncurry5 f $ getInput5 f1 f2 f3 f4 f5 s

uncurry2 :: (a -> b -> c) -> ((a, b) -> c)
uncurry2 f (a,b) = f a b

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a,b,c,d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f (a,b,c,d,e) = f a b c d e

setOutput1 :: ASetter' s a -> s -> a -> s
setOutput1 f1 s a = set f1 a s

setOutput2 :: ASetter' s a -> ASetter' s b -> s -> (a, b) -> s
setOutput2 f1 f2 s (a, b) = set f1 a $ set f2 b s

setOutput3 :: ASetter' s a -> ASetter' s b -> ASetter' s c -> s -> (a, b, c) -> s
setOutput3 f1 f2 f3 s (a, b, c) = set f1 a $ set f2 b $ set f3 c s
