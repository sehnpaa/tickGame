{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text (concat, pack, Text)

import Mod

data MyState = MyState
  { actions :: [Action]
  , errorLog :: [Text]
  , paperclips :: Integer
  , helpers :: Integer
  , treeSeeds :: Integer
  , seconds :: Integer
  , isStarted :: Bool } deriving (Eq, Show)

data MyEvent
  = Start
  | CreatePC
  | CreateHelper
  | PlantASeed
  | ExitApplication
  | Tick
  deriving (Eq, Show)

data Action = CreateHelperAction deriving (Eq, Show)

nextTick :: MyState -> MyState
nextTick (MyState as es p h t s True) = MyState as es (p+h*2) h t (succ s) True

buyHelper :: MyState -> MyState
buyHelper (MyState as es p h t s True) =
  let price = 10 in
    if p < price
      then MyState as (es ++ [Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]]) p h t s True
      else MyState as es (p - price) (succ h) t s True

plantASeed :: MyState -> MyState
plantASeed (MyState as es p h t s True) = MyState as es p h (pred t) s True