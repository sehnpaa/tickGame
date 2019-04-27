{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Data.Functor
import Data.Text (concat, pack, Text)
import Lens.Micro.Platform

import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork

helperWork :: MyState -> MyState
helperWork state =
  let h = view helpers state
  in over paperclips (addHelperWork h) state

addHelperWork :: Helpers -> Paperclips -> Paperclips
addHelperWork h p = Paperclips $ (unPaperclips p) + (unHelpers h) * 2

addSecond :: MyState -> MyState
addSecond = over seconds (+1)

createPC :: MyState -> MyState
createPC = over paperclips (+1)

buyHelper :: MyState -> MyState
buyHelper (MyState as es p h t s True) =
  let price = 10 in
    if p < price
      then MyState as (es ++ [Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]]) p h t s True
      else MyState as es (p - price) (succ h) t s True

plantASeed :: MyState -> MyState
plantASeed (MyState as es p h t s True) = MyState as es p h (pred t) s True