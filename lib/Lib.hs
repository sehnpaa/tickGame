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


g = f