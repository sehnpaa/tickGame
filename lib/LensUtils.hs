module LensUtils where

import           Control.Lens                   ( Getting
                                                , view
                                                )

arg1 :: (a -> b) -> Getting a s a -> s -> b
arg1 f f1 s = f (view f1 s)

arg2 :: (a -> b -> c) -> Getting a s a -> Getting b s b -> s -> c
arg2 f f1 f2 s = arg1 f f1 s (view f2 s)

arg3
  :: (a -> b -> c -> d)
  -> Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> s
  -> d
arg3 f f1 f2 f3 s = arg2 f f1 f2 s (view f3 s)

arg4
  :: (a -> b -> c -> d -> e)
  -> Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> s
  -> e
arg4 f f1 f2 f3 f4 s = arg3 f f1 f2 f3 s (view f4 s)

arg5
  :: (a -> b -> c -> d -> e -> f)
  -> Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> s
  -> f
arg5 f f1 f2 f3 f4 f5 s = arg4 f f1 f2 f3 f4 s (view f5 s)
