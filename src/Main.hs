{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Vector (Vector, fromList)
import Data.Text (append, pack, Text)
import GI.Gtk (Box(..), Button(..)
  , Dialog(..), Label(..), ListBox(..), ListBoxRow(..), Orientation(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Lib (buyHelper, nextTick, plantASeed, MyEvent(..), MyState(..))

main :: IO ()
main = void $ run app

view' :: MyState -> AppView Window MyEvent
view' state@(MyState actions errorlog paperclips helpers treeSeeds seconds isStarted) = bin Window
      [ #title := "Hello"
      , on #deleteEvent (const (True, ExitApplication))
      , #widthRequest := 600
      , #heightRequest := 300
      ]
      $ container Box [#orientation := OrientationVertical]
        [ container Box [#orientation := OrientationHorizontal] [widget Label [#label := "tickGame"]]
        , container Box [#orientation := OrientationHorizontal, #widthRequest := 300]
          [ buttons
          , margin
          , stats state]
        , container ListBox [] (fromList errorlog <&> \name -> bin ListBoxRow [#activatable := False, #selectable := False] $ widget Label [#label := name])
        , container Box [#orientation := OrientationVertical] [widget Label [#label := "here"]]]

buttons :: BoxChild MyEvent
buttons = container Box [#orientation := OrientationVertical, #widthRequest := 150]
  [ widget Button [#label := "Start game", on #clicked Start]
  , widget Button [#label := "Create paperclip", on #clicked CreatePC]
  , widget Button [#label := "Create helper", on #clicked CreateHelper]
  , widget Button [#label := "Plant a seed", on #clicked PlantASeed]
  , widget Button [#label := "Exit", on #clicked ExitApplication]]

margin :: BoxChild MyEvent
margin = container Box [#widthRequest := 10] []

stats :: MyState -> BoxChild MyEvent
stats state = container Box [#orientation := OrientationVertical]
  [ statProperty "Paperclips" (paperclips state)
  , statProperty "Helpers" (helpers state)
  , statProperty "Tree seeds" (treeSeeds state)
  , statProperty "Seconds" (seconds state) ]

statProperty :: Show a => Text -> a -> BoxChild MyEvent
statProperty labelText n = container Box [#orientation := OrientationHorizontal]
  [ widget Label [#label := (append labelText ": ")], widget Label [#label := (pack . show $ n)]]


ticker :: IO (Maybe MyEvent)
ticker = fmap (const (Just Tick)) (threadDelay 1000000)

update' :: MyState -> MyEvent -> Transition MyState MyEvent
update' (MyState as el p h t s False) Start = Transition (MyState as el p h t s True) ticker
update' (MyState as el p h t s True) CreatePC = Transition (MyState as el (succ p) h t s True) (pure Nothing)
update' state CreateHelper = Transition (buyHelper state) (pure Nothing)
update' state PlantASeed = Transition (plantASeed state) (pure Nothing)
update' state Tick = Transition (nextTick state) ticker
update' state ExitApplication = Exit
update' state _ = Transition state (pure Nothing)

app :: App Window MyState MyEvent
app = App
  { view = view'
  , update = update'
  , inputs = []
  , initialState = MyState [] [] 0 0 10 0 False}