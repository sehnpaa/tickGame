{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( void )
import           Data.ByteString                ( ByteString )
import           Data.Functor                   ( (<&>) )
import           Data.Vector                    ( Vector
                                                , fromList
                                                , mapMaybe
                                                )
import           Data.Text                      ( Text
                                                , append
                                                , pack
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
                                                , extendStorage
                                                , generateEnergy
                                                , nextTick
                                                , pumpWater
                                                , researchAdvancedHelper
                                                , previousSnapshot
                                                , nextSnapshot
                                                , applySnapshot
                                                , setStarted
                                                , unErrorLogLine
                                                , plantASeed
                                                , Button(..)
                                                , ButtonEvent(..)
                                                , ButtonTitle(..)
                                                , ButtonStatus(..)
                                                , ButtonData(..)
                                                , ButtonDataAPI(..)
                                                , HasAcquirePaperclips
                                                , HasButtonDisplayStatus
                                                , HasBuyTreeSeeds
                                                , HasCostEnergyPaperclips
                                                , HasDurationTreeSeeds
                                                , HasEnergy
                                                , HasEnergyErrorMessage
                                                , HasEventCreatePaperclip
                                                , HasEventStart
                                                , HasHelpers
                                                , HasHelperInc
                                                , HasPaperclips
                                                , HasPaperclipsErrorMessage
                                                , HasPrices
                                                , HasResearchProgress
                                                , HasResearchComp
                                                , HasSeconds
                                                , HasSource
                                                , HasState
                                                , HasStorageManually
                                                , HasStorageOfPaperclips
                                                , HasTreeSeedCostPerTick
                                                , HasTreeSeeds
                                                , HasTrees
                                                , HasWater
                                                , HasWaterTank
                                                , HasWood
                                                , IsStarted(..)
                                                , MyEvent(..)
                                                , State(..)
                                                , Status(..)
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

view'
  :: (Eq s, Num s, Show s)
  => State s
  -> AppView Window MyEvent
view' state =
  bin
      Window
      [ #title := (pack . show $ viewTitle state)
      , on #deleteEvent (const (True, ExitApplication))
      , #widthRequest := 600
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ container Box
                    [#orientation := OrientationHorizontal]
                    [widget Label [#label := (pack . show $ viewTitle state)]]
        , container
          Box
          [#orientation := OrientationHorizontal, #widthRequest := 300]
          [buttons state, margin, stats state]
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
        , widget
          Entry
          [ #text := (pack . show $ viewSource state)
          , onM #changed requestCompilation
          ]
        , widget Label [#label := (pack . show $ viewSourceStatus state)]
        ]

requestCompilation :: Entry -> IO MyEvent
requestCompilation c = fmap Compile (entryGetBuffer c >>= entryBufferGetText)

createButtons
  :: (Show s) => State s
  -> Vector (BoxChild MyEvent)
createButtons state = mapMaybe createButton
  [ viewButtonData ButtonStart                  state
  , viewButtonData ButtonCreatePaperclip        state
  , viewButtonData ButtonCreateHelper           state
  , viewButtonData ButtonExtendStorage          state
  , viewButtonData ButtonPumpWater              state
  , viewButtonData ButtonGenerateEnergy         state
  , viewButtonData ButtonBuyASeed               state
  , viewButtonData ButtonPlantASeed             state
  , viewButtonData ButtonResearchAdvancedHelper state
  , viewButtonData ButtonPreviousSnapshot       state
  , viewButtonData ButtonNextSnapshot           state
  , viewButtonData ButtonApplySnapshot          state
  , viewButtonData ButtonExitApplication        state
  ]

createButton :: ButtonDataAPI -> Maybe (BoxChild MyEvent)
createButton (ButtonDataAPI (ButtonData (ButtonTitle t) (ButtonStatus Enabled) (ButtonEvent e) _) str)
  = Just $ widget
    Button
    [ #label := t
    , on #clicked e
    , #tooltipMarkup
      := (        "<span foreground=\"white\" size=\"medium\">"
         `append` pack str
         `append` "</span>"
         )
    ]
createButton (ButtonDataAPI (ButtonData (ButtonTitle t) (ButtonStatus Disabled) _ _) str)
  = Just $ widget
    Button
    [ #label := t
    , #sensitive := False
    , #tooltipMarkup
      := (        "<span foreground=\"white\" size=\"medium\">"
         `append` pack str
         `append` "</span>"
         )
    ]
createButton (ButtonDataAPI (ButtonData _ (ButtonStatus Hidden) _ _) _) =
  Nothing

buttons
  :: Show s
  => State s
  -> BoxChild MyEvent
buttons state = container
  Box
  [#orientation := OrientationVertical, #widthRequest := 150]
  (createButtons state)

margin :: BoxChild MyEvent
margin = container Box [#widthRequest := 10] []

stats :: (Eq a, Num a, Show a, HasHelpers s a, HasState s a) => s -> BoxChild MyEvent
stats state = container Box
                        [#orientation := OrientationVertical]
                        [statProperty "Paperclips" (viewPaperclips state)
  , statProperty "Helpers"                  (viewHelpers state)
  , statProperty "Storage of paperclips"     (viewStorageOfPaperclips state)
  , statProperty "Water"                    (viewWater state)
  , statProperty "Water tank"               (viewWaterTank state)
  , statProperty "Energy"                   (viewEnergy state)
  , statProperty "Tree seeds"               (viewTreeSeeds state)
  , statProperty "Trees"                    (viewTrees state)
  , statProperty "Wood"                     (viewWood state)
  , statProperty "Advanced helper research" (viewAdvancedHelperResearch state)
  , statProperty "Seconds"                  (viewSeconds state)
  , statProperty "Snapshots"                (viewSnapshots state)
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

update'
  :: ( Enum a
     , Integral a
     , Num a
     , Ord a
     , Show a
     , HasBuyTreeSeeds s a
     , HasCostEnergyPaperclips s a
     , HasDurationTreeSeeds s a
     , HasEnergy s a
     , HasEnergyErrorMessage s
     , HasHelperInc s a
     , HasHelpers s a
     , HasPaperclips s a
     , HasPaperclipsErrorMessage s
     , HasPrices s a
     , HasResearchComp s a
     , HasResearchProgress s a
     , HasSeconds s a
     , HasSource s a
     , HasState s a
     , HasStorageManually s a
     , HasStorageOfPaperclips s a
     , HasTreeSeedCostPerTick s a
     , HasTreeSeeds s a
     , HasTrees s a
     , HasWater s a
     , HasWaterTank s a
     , HasWood s a
     )
  => s
  -> MyEvent
  -> Transition s MyEvent
update' state event = case (unIsStarted (viewIsStarted state), event) of
  (False, Start          ) -> Transition (setStarted True state) ticker
  (True , Start          ) -> Transition (setStarted False state) ticker
  (_    , ExitApplication) -> Exit
  (True , BuyASeed       ) -> Transition (buyASeed state) (pure Nothing)
  (True , CreatePaperclip) -> Transition (createPaperclip state) (pure Nothing)
  (True , CreateHelper   ) -> Transition (buyHelper state) (pure Nothing)
  (True , ExtendStorage  ) -> Transition (extendStorage state) (pure Nothing)
  (True , GenerateEnergy ) -> Transition (generateEnergy state) (pure Nothing)
  (True , PumpWater      ) -> Transition (pumpWater state) (pure Nothing)
  (True , PlantASeed     ) -> Transition (plantASeed state) (pure Nothing)
  (True, ResearchAdvancedHelper) ->
    Transition (researchAdvancedHelper state) (pure Nothing)

  -- Snapshots can only be managed when the game is not running
  (False, PreviousSnapshot) ->
    Transition (previousSnapshot state) (pure Nothing)
  (True , PreviousSnapshot) -> Transition state (pure Nothing)
  (False, NextSnapshot    ) -> Transition (nextSnapshot state) (pure Nothing)
  (True , NextSnapshot    ) -> Transition state (pure Nothing)
  (False, ApplySnapshot   ) -> Transition (applySnapshot state) (pure Nothing)
  (True , ApplySnapshot   ) -> Transition state (pure Nothing)

  (True , Tick            ) -> Transition (nextTick state) ticker
  (True , Compile t       ) -> Transition (compile t state) (pure Nothing)
  (False, _               ) -> Transition state (pure Nothing)


app :: App Window (State Integer) MyEvent
app = App { view         = view'
          , update       = update'
          , inputs       = []
          , initialState = getInitialState
          }
