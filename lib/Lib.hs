{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Data.Functor
import Data.Text (concat, pack, Text)
import Lens.Micro.Platform

import Mod

nextTick :: MyState -> MyState
nextTick (MyState as es p h t s True) = MyState as es (p+h*2) h t (succ s) True

createPC :: MyState -> MyState
createPC (MyState as el p h t s True) = MyState as el (succ p) h t s True

buyHelper :: MyState -> MyState
buyHelper (MyState as es p h t s True) =
  let price = 10 in
    if p < price
      then MyState as (es ++ [Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]]) p h t s True
      else MyState as es (p - price) (succ h) t s True

plantASeed :: MyState -> MyState
plantASeed (MyState as es p h t s True) = MyState as es p h (pred t) s True