module Lib where

import Mod

data MyState = MyState
  { actions :: [Action]
  , paperclips :: Integer
  , helpers :: Integer
  , seconds :: Integer
  , isStarted :: Bool }

data MyEvent
  = Start
  | CreatePC
  | CreateHelper
  | Dec
  | ExitApplication
  | Tick

data Action = CreateHelperAction

nextTick :: MyState -> MyState
nextTick (MyState as p h s True) = MyState as (p+h*10) h (succ s) True

buyHelper :: MyState -> MyState
buyHelper (MyState as p h s True) = MyState as (p-100) (succ h) s True