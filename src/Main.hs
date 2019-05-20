{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Functor ((<&>))
import Data.Vector (fromList)
import Data.Text (append, pack, Text)
import GI.Gtk (Box(..), Button(..), Label(..), ListBox(..), ListBoxRow(..), Orientation(..), Window(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Lib (buyHelper, createPC, nextTick, researchAdvancedHelper, setStarted, viewAdvancedHelperResearch, viewErrorLog, viewHelpers, viewIsStarted
  , viewPaperclips, viewSeconds, viewStorage, viewTrees, viewTreeSeeds, unErrorLogLine, plantASeed, IsStarted(..), MyEvent(..)
  , MyState(..), getInitialState)

main :: IO ()
main = void $ run app

view' :: MyState -> AppView Window MyEvent
view' state = bin Window
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
        , container ListBox [] (fromList (viewErrorLog state) <&> \name -> bin ListBoxRow [#activatable := False, #selectable := False] $ widget Label [#label := unErrorLogLine name])
        , container Box [#orientation := OrientationVertical] [widget Label [#label := "here"]]]

buttons :: BoxChild MyEvent
buttons = container Box [#orientation := OrientationVertical, #widthRequest := 150]
  [ widget Button [#label := "Start game", on #clicked Start]
  , widget Button [#label := "Create paperclip", on #clicked CreatePC]
  , widget Button [#label := "Create helper", on #clicked CreateHelper]
  , widget Button [#label := "Research advanced helper", on #clicked ResearchAdvancedHelper]
  , widget Button [#label := "Plant a seed", on #clicked PlantASeed]
  , widget Button [#label := "Exit", on #clicked ExitApplication]]

margin :: BoxChild MyEvent
margin = container Box [#widthRequest := 10] []

stats :: MyState -> BoxChild MyEvent
stats state = container Box [#orientation := OrientationVertical]
  [ statProperty "Paperclips" (viewPaperclips state)
  , statProperty "Helpers" (viewHelpers state)
  , statProperty "Storage" (viewStorage state)
  , statProperty "Tree seeds" (viewTreeSeeds state)
  , statProperty "Trees" (viewTrees state)
  , statProperty "Advanced helper research" (viewAdvancedHelperResearch state)
  , statProperty "Seconds" (viewSeconds state) ]

statProperty :: Show a => Text -> a -> BoxChild MyEvent
statProperty labelText n = container Box [#orientation := OrientationHorizontal]
  [ widget Label [#label := (append labelText ": ")], widget Label [#label := (pack . show $ n)]]

ticker :: IO (Maybe MyEvent)
ticker = fmap (const (Just Tick)) (threadDelay 1000000)

update' :: MyState -> MyEvent -> Transition MyState MyEvent
update' state event = case (unIsStarted (viewIsStarted state), event) of
  (False, Start) -> Transition (setStarted state) ticker
  (_, ExitApplication) -> Exit
  (True, CreatePC) -> Transition (createPC state) (pure Nothing)
  (True, CreateHelper) -> Transition (buyHelper state) (pure Nothing)
  (True, ResearchAdvancedHelper) -> Transition (researchAdvancedHelper state) (pure Nothing)
  (True, PlantASeed) -> Transition (plantASeed state) (pure Nothing)
  (True, Start) -> Transition state (pure Nothing)
  (True, Tick) -> Transition (nextTick state) ticker
  (False, _) -> Transition state (pure Nothing)

app :: App Window MyState MyEvent
app = App
  { view = view'
  , update = update'
  , inputs = []
  , initialState = getInitialState }