{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( void )
import           Data.ByteString                ( ByteString )
import           Data.Functor                   ( (<&>) )
import           Data.Vector                    ( fromList )
import           Data.Text                      ( append
                                                , pack
                                                , Text
                                                )
import qualified GI.Gdk                        as Gdk
import           GI.Gtk                         ( Box(..)
                                                , Button(..)
                                                , Entry(..)
                                                , Label(..)
                                                , ListBox(..)
                                                , ListBoxRow(..)
                                                , Orientation(..)
                                                , Window(..)
                                                , cssProviderLoadFromData
                                                , cssProviderNew
                                                , styleContextAddProviderForScreen
                                                )
import qualified GI.Gtk                        as Gtk

import           GI.Gtk.Objects.Entry           ( entryGetBuffer )
import           GI.Gtk.Objects.EntryBuffer     ( entryBufferGetText )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple  ( App(..)
                                                , AppView
                                                , Transition(..)
                                                , runLoop
                                                , update
                                                , view
                                                )

import           Lib                            ( buyASeed
                                                , buyHelper
                                                , compile
                                                , createPaperclip
                                                , generateEnergy
                                                , nextTick
                                                , pumpWater
                                                , researchAdvancedHelper
                                                , setStarted
                                                , unErrorLogLine
                                                , plantASeed
                                                , IsStarted(..)
                                                , MyEvent(..)
                                                , State(..)
                                                , getInitialState
                                                )
import           View

main :: IO ()
main = do
  void $ Gtk.init Nothing

  -- Set up screen and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- cssProviderNew
  cssProviderLoadFromData p styles
  styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main

styles :: ByteString
styles = mconcat
  [ "button { border: 2px solid gray; font-weight: 800; }"
  , ".selected { background: white; border: 2px solid black; }"
  , ".red { color: red; }"
  , ".green { color: green; }"
  , ".blue { color: blue; }"
  , ".yellow { color: goldenrod; }"
  ]

view' :: State Integer -> AppView Window MyEvent
view' state =
  bin
      Window
      [ #title := "Hello"
      , on #deleteEvent (const (True, ExitApplication))
      , #widthRequest := 600
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ container Box
                    [#orientation := OrientationHorizontal]
                    [widget Label [#label := "tickGame"]]
        , container
          Box
          [#orientation := OrientationHorizontal, #widthRequest := 300]
          [buttons, margin, stats state]
        , container
          ListBox
          []
          (fromList (viewErrorLog state) <&> \name ->
            bin ListBoxRow [#activatable := False, #selectable := False]
              $ widget Label [#label := unErrorLogLine name]
          )
        , container Box
                    [#orientation := OrientationVertical]
                    [widget Label [#label := "here"]]
        , widget Entry
                 [#text := viewSource state, onM #changed requestCompilation]
        , widget Label [#label := viewSourceStatus state]
        ]

requestCompilation :: Entry -> IO MyEvent
requestCompilation c = fmap Compile (entryGetBuffer c >>= entryBufferGetText)

buttons :: BoxChild MyEvent
buttons = container
  Box
  [#orientation := OrientationVertical, #widthRequest := 150]
  [ widget Button [#label := "Start game", on #clicked Start]
  , widget Button [#label := "Create paperclip", on #clicked CreatePaperclip]
  , widget Button [#label := "Create helper", on #clicked CreateHelper]
  , widget Button [#label := "Pump water", on #clicked PumpWater]
  , widget Button [#label := "Generate energy", on #clicked GenerateEnergy]
  , widget Button [#label := "Buy a seed", on #clicked BuyASeed]
  , widget Button [#label := "Plant a seed", on #clicked PlantASeed]
  , widget
    Button
    [#label := "Research advanced helper", on #clicked ResearchAdvancedHelper]
  , widget Button [#label := "Exit", on #clicked ExitApplication]
  ]

margin :: BoxChild MyEvent
margin = container Box [#widthRequest := 10] []

stats :: State Integer -> BoxChild MyEvent
stats state = container
  Box
  [#orientation := OrientationVertical]
  [ statProperty "Paperclips"               (viewPaperclips state)
  , statProperty "Helpers"                  (viewHelpers state)
  , statProperty "Storage"                  (viewStorage state)
  , statProperty "Water"                    (viewWater state)
  , statProperty "Water tank"               (viewWaterTank state)
  , statProperty "Energy"                   (viewEnergy state)
  , statProperty "Tree seeds"               (viewTreeSeeds state)
  , statProperty "Trees"                    (viewTrees state)
  , statProperty "Wood"                     (viewWood state)
  , statProperty "Advanced helper research" (viewAdvancedHelperResearch state)
  , statProperty "Seconds"                  (viewSeconds state)
  ]

statProperty :: Show a => Text -> a -> BoxChild MyEvent
statProperty labelText n = container
  Box
  [#orientation := OrientationHorizontal]
  [ widget Label [#label := (append labelText ": ")]
  , widget Label [#label := (pack . show $ n)]
  ]

ticker :: IO (Maybe MyEvent)
ticker = fmap (const (Just Tick)) (threadDelay 1000000)

update' :: State Integer -> MyEvent -> Transition (State Integer) MyEvent
update' state event = case (unIsStarted (viewIsStarted state), event) of
  (False, Start          ) -> Transition (setStarted state) ticker
  (_    , ExitApplication) -> Exit
  (True , BuyASeed       ) -> Transition (buyASeed state) (pure Nothing)
  (True , CreatePaperclip) -> Transition (createPaperclip state) (pure Nothing)
  (True , CreateHelper   ) -> Transition (buyHelper state) (pure Nothing)
  (True , GenerateEnergy ) -> Transition (generateEnergy state) (pure Nothing)
  (True , PumpWater      ) -> Transition (pumpWater state) (pure Nothing)
  (True , PlantASeed     ) -> Transition (plantASeed state) (pure Nothing)
  (True, ResearchAdvancedHelper) ->
    Transition (researchAdvancedHelper state) (pure Nothing)
  (True , Start    ) -> Transition state (pure Nothing)
  (True , Tick     ) -> Transition (nextTick state) ticker
  (True , Compile t) -> Transition (compile t state) (pure Nothing)
  (False, _        ) -> Transition state (pure Nothing)

app :: App Window (State Integer) MyEvent
app = App { view         = view'
          , update       = update'
          , inputs       = []
          , initialState = getInitialState
          }
