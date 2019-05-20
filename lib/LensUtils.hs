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

setOutput1 :: ASetter' s a -> s -> a -> s
setOutput1 f1 s a = set f1 a s

setOutput2 :: ASetter' s a -> ASetter' s b -> s -> (a, b) -> s
setOutput2 f1 f2 s (a, b) = set f1 a $ set f2 b s

setOutput3 :: ASetter' s a -> ASetter' s b -> ASetter' s c -> s -> (a, b, c) -> s
setOutput3 f1 f2 f3 s (a, b, c) = set f1 a $ set f2 b $ set f3 c s
