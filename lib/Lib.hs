module Lib where

import Data.Text (Text)

import Mod

data MyState = MyState
  { actions :: [Action]
  , errorLog :: [Text]
  , paperclips :: Integer
  , helpers :: Integer
  , seconds :: Integer
  , isStarted :: Bool } deriving (Eq, Show)

data MyEvent
  = Start
  | CreatePC
  | CreateHelper
  | Dec
  | ExitApplication
  | Tick
  deriving (Eq, Show)

data Action = CreateHelperAction deriving (Eq, Show)

nextTick :: MyState -> MyState
nextTick (MyState as [] p h s True) = MyState as [] (p+h*2) h (succ s) True

buyHelper :: MyState -> MyState
buyHelper (MyState as [] p h s True) = MyState as [] (p-10) (succ h) s True