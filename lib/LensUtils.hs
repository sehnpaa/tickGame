module LensUtils where

import           Control.Lens                   ( Getting
                                                , view
                                                )

get2 :: Getting a s a -> Getting b s b -> s -> (a, b)
get2 a b s = (view a s, view b s)

get3 :: Getting a s a -> Getting b s b -> Getting c s c -> s -> (a, b, c)
get3 a b c s = (view a s, view b s, view c s)

get4
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> s
  -> (a, b, c, d)
get4 a b c d s = (view a s, view b s, view c s, view d s)

get5
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> s
  -> (a, b, c, d, e)
get5 a b c d e s = (view a s, view b s, view c s, view d s, view e s)