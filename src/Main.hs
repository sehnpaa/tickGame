{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Text (append, pack, Text)
import GI.Gtk (Box(..), Button(..)
  , Dialog(..), Label(..), Orientation(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

main :: IO ()
main = void $ run app

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

view' :: MyState -> AppView Window MyEvent
view' state@(MyState actions paperclips helpers seconds isStarted) = bin Window
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
        , container Box [#orientation := OrientationVertical] [widget Label [#label := "here"]]]
        -- , container Box [#orientation := OrientationHorizontal] [widget Label [#label := pack (show paperclips)]]
        -- , container Box [#orientation := OrientationVertical] [widget Label [#label := pack (show helpers)]]]
      -- $ paned [#wideHandle := True]
      -- (pane paneProps $ widget Button [#label := "Left", on #clicked Inc])
      --pane paneProps $ widget Label [#label := pack (show n)])
        where paneProps = defaultPaneProperties { resize = True }

buttons :: BoxChild MyEvent
buttons = container Box [#orientation := OrientationVertical, #widthRequest := 150]
  [ widget Button [#label := "Start game", on #clicked Start]
  , widget Button [#label := "Create paperclip", on #clicked CreatePC]
  , widget Button [#label := "Create helper", on #clicked CreateHelper]
  , widget Button [#label := "Exit", on #clicked ExitApplication]]

margin :: BoxChild MyEvent
margin = container Box [#widthRequest := 10] []

stats :: MyState -> BoxChild MyEvent
stats state = container Box [#orientation := OrientationVertical]
  [ statProperty "Paperclips" (paperclips state)
  , statProperty "Helpers" (helpers state)
  , statProperty "Seconds" (seconds state) ]

statProperty :: Show a => Text -> a -> BoxChild MyEvent
statProperty labelText n = container Box [#orientation := OrientationHorizontal]
  [ widget Label [#label := (append labelText ": ")], widget Label [#label := (pack . show $ n)]]

ticker :: IO (Maybe MyEvent)
ticker = fmap (const (Just Tick)) (threadDelay 1000000)

update' :: MyState -> MyEvent -> Transition MyState MyEvent
update' (MyState as p h s False) Start = Transition (MyState as p h s True) ticker
update' (MyState as p h s True) CreatePC = Transition (MyState as (succ p) h s True) (pure Nothing)
update' state CreateHelper = Transition (buyHelper state) (pure Nothing)
update' state Tick = Transition (nextTick state) ticker
update' (MyState as p h s _) Dec = Exit
update' state ExitApplication = Exit
update' state _ = Transition state (pure Nothing)

nextTick :: MyState -> MyState
nextTick (MyState as p h s True) = MyState as (p+h*10) h (succ s) True

buyHelper :: MyState -> MyState
buyHelper (MyState as p h s True) = MyState as (p-100) (succ h) s True

--initTicker :: Producer MyEvent IO ()
--initTicker = undefined

app :: App Window MyState MyEvent
app = App
  { view = view'
  , update = update'
  , inputs = []
  , initialState = MyState [] 0 0 0 False}