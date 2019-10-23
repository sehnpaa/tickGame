{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( makeClassy
                                                , makeClassyPrisms
                                                , over
                                                , set
                                                , view
                                                )
import           Data.List.Zipper               ( Zipper(..)
                                                , safeCursor
                                                )
import           Data.Text                      ( Text
                                                , concat
                                                , pack
                                                , unpack
                                                )

import           Config
import           Elements
import           Iso
import           Resources
import           Seconds
import           Source
import           Utils

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text }

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

data ResearchProgress a = NotResearched | ResearchInProgress a | ResearchDone
  deriving (Eq)

instance Show (ResearchProgress Integer) where
  show NotResearched = "Not researched"
  show (ResearchInProgress n) =
    "Research in progress - " ++ show n ++ " " ++ noun n ++ " left."
   where
    noun 1 = "tick"
    noun _ = "ticks"
  show ResearchDone = "Research done"

newtype DurationAdvancedHelper a = DurationAdvanced { unDurationAdvanced :: Duration a }

data ResearchComp a = ResearchComp
  { _researchCompDuration :: DurationAdvancedHelper a
  , _researchCompProgress :: ResearchProgress a
  , _researchCompErrorMessage :: Text
  , _researchCompInProgressErr :: Text
  , _researchCompDoneErr :: Text }
makeClassy ''ResearchComp

data ResearchAreas a = ResearchAreas
  { _advancedHelperResearch :: ResearchComp a}
makeClassy ''ResearchAreas

newtype IsStarted = IsStarted { unIsStarted :: Bool }

instance Show IsStarted where
  show (IsStarted a) = show a

data Status = Hidden | Disabled | Enabled deriving Show

data MyEvent
  = Start
  | CreatePaperclip
  | CreateHelper
  | ExtendStorage
  | GenerateEnergy
  | PumpWater
  | PlantASeed
  | BuyASeed
  | ResearchAdvancedHelper
  | PreviousSnapshot
  | NextSnapshot
  | ApplySnapshot
  | ExitApplication
  | Tick
  | Compile Text
  deriving (Eq, Show)

data Action a
  = SetP (Paperclips a)
  | SetH (Helpers a)
  | SetE ErrorLogLine
  | SetEnergy (Energy a)
  | SetR (ResearchProgress a)
  | SetStorageOfPaperclips (StorageOfPaperclips a)
  | SetTreeSeeds (TreeSeeds a)
  | SetTrees (Trees a)
  | SetWater (Water a)
  | SetWood (Wood a)
  | SetAdvancedHelperResearchProgress (ResearchProgress a)
  | SetHelperInc (HelperInc (Helpers a))
  | SetProgs [Prog a]
makeClassyPrisms ''Action

instance Show (Action Integer) where
  show (SetP                              a            ) = show a
  show (SetH                              a            ) = show a
  show (SetE                              a            ) = show a
  show (SetEnergy                         a            ) = show a
  show (SetR                              a            ) = show a
  show (SetStorageOfPaperclips            a            ) = show a
  show (SetTreeSeeds                      a            ) = show a
  show (SetTrees                          a            ) = show a
  show (SetWater                          a            ) = show a
  show (SetWood                           a            ) = show a
  show (SetAdvancedHelperResearchProgress a            ) = show a
  show (SetHelperInc                      (HelperInc a)) = show a
  show (SetProgs                          a            ) = show a

newtype ButtonTitle = ButtonTitle Text

instance Show ButtonTitle where
  show (ButtonTitle t) = unpack t

newtype ButtonStatus = ButtonStatus Status deriving Show

newtype ButtonEvent = ButtonEvent MyEvent deriving Show

data ButtonData = ButtonData
  { _buttonTitle :: ButtonTitle
  , _buttonStatus :: ButtonStatus
  , _buttonEvent :: ButtonEvent } deriving Show
makeClassy ''ButtonData

newtype EventStart =
  EventStart { _eventStartButtonData :: ButtonData } deriving Show
makeClassy ''EventStart

newtype EventCreatePaperclip =
  EventCreatePaperclip { _eventCreatePaperclipButtonData :: ButtonData } deriving Show
makeClassy ''EventCreatePaperclip

newtype EventCreateHelper =
  EventCreateHelper { _eventCreateHelperButtonData :: ButtonData } deriving Show
makeClassy ''EventCreateHelper

newtype EventExtendStorage =
  EventExtendStorage { _eventExtendStorageButtonData :: ButtonData } deriving Show
makeClassy ''EventExtendStorage

newtype EventPumpWater =
  EventPumpWater { _eventPumpWaterButtonData :: ButtonData } deriving Show
makeClassy ''EventPumpWater

newtype EventGenerateEnergy =
  EventGenerateEnergy { _eventGenerateEnergyButtonData :: ButtonData } deriving Show
makeClassy ''EventGenerateEnergy

newtype EventBuyASeed =
  EventBuyASeed { _eventBuyASeedButtonData :: ButtonData } deriving Show
makeClassy ''EventBuyASeed

newtype EventPlantASeed =
  EventPlantASeed { _eventPlantASeedButtonData :: ButtonData } deriving Show
makeClassy ''EventPlantASeed

newtype EventResearchAdvancedHelper =
  EventResearchAdvancedHelper { _eventResearchAdvancedHelperButtonData :: ButtonData } deriving Show
makeClassy ''EventResearchAdvancedHelper

newtype EventPreviousSnapshot =
  EventPreviousSnapshot { _eventPreviousSnapshotButtonData :: ButtonData } deriving Show
makeClassy ''EventPreviousSnapshot

newtype EventNextSnapshot =
  EventNextSnapshot { _eventNextSnapshotButtonData :: ButtonData } deriving Show
makeClassy ''EventNextSnapshot

newtype EventApplySnapshot =
  EventApplySnapshot { _eventApplySnapshotButtonData :: ButtonData } deriving Show
makeClassy ''EventApplySnapshot

newtype EventExitApplication =
  EventExitApplication { _eventExitApplicationButtonData :: ButtonData } deriving Show
makeClassy ''EventExitApplication

data Events = Events
  { _eventsEventStart :: EventStart
  , _eventsEventCreatePaperclip :: EventCreatePaperclip
  , _eventsEventCreateHelper :: EventCreateHelper
  , _eventsEventExtendStorage :: EventExtendStorage
  , _eventsEventPumpWater :: EventPumpWater
  , _eventsEventGenerateEnergy :: EventGenerateEnergy
  , _eventsEventBuyASeed :: EventBuyASeed
  , _eventsEventPlantASeed :: EventPlantASeed
  , _eventsEventResearchAdvancedHelper :: EventResearchAdvancedHelper
  , _eventsEventPreviousSnapshot :: EventPreviousSnapshot
  , _eventsEventNextSnapshot :: EventNextSnapshot
  , _eventsEventApplySnapshot :: EventApplySnapshot
  , _eventsEventExitApplication :: EventExitApplication } deriving Show
makeClassy ''Events

data Button
  = ButtonStart
  | ButtonCreatePaperclip
  | ButtonCreateHelper
  | ButtonExtendStorage
  | ButtonPumpWater
  | ButtonGenerateEnergy
  | ButtonBuyASeed
  | ButtonPlantASeed
  | ButtonResearchAdvancedHelper
  | ButtonPreviousSnapshot
  | ButtonNextSnapshot
  | ButtonApplySnapshot
  | ButtonExitApplication

newtype Snapshots a = Snapshots { _zipper :: Zipper (Resources a)}
makeClassy ''Snapshots

instance Show (Snapshots Integer) where
  show (Snapshots rs) = case safeCursor rs of
    Nothing -> "Nothing to show yet."
    Just x ->
      let y = view (resourcesElements . elementsPaperclips . count) x
      in  "Paperclips in focus of snapshot: " ++ show y

newtype Title = Title Text

instance Show Title where
  show (Title t) = unpack t

data State a = State
  { _stateConfig :: Config a
  , _stateActions :: [Action a]
  , _stateErrorLog :: [ErrorLogLine]
  , _stateEvents :: Events
  , _stateResearchAreas :: ResearchAreas a
  , _stateResources :: Resources a
  , _stateSeconds :: Seconds a
  , _stateSnapshots :: Snapshots a
  , _stateSource :: Source a
  , _stateTitle :: Title
  , _stateIsStarted :: IsStarted }
makeClassy ''State

instance HasPaperclips (State a) a where
  paperclips = stateResources . resourcesElements . elementsPaperclips . count

instance HasStorageOfPaperclips (State a) a where
  storageOfPaperclips = stateResources . resourcesStorage

instance HasTreeSeeds (State a) a where
  treeSeeds = stateResources . resourcesElements . elementsTreeSeeds . count

instance HasDurationTreeSeeds (State a) a where
  durationTreeSeeds = stateResources . resourcesElements . elementsTreeSeeds . duration

instance HasSeconds (State a) a where
  seconds = stateSeconds

applyAction :: HasState t a => Action a -> t -> t
applyAction (SetP p) =
  set (stateResources . resourcesElements . elementsPaperclips . count) p
applyAction (SetH h) =
  set (stateResources . resourcesElements . elementsHelpers . count) h
applyAction (SetE err) = over stateErrorLog (\errs -> err : errs)
applyAction (SetEnergy e) =
  set (stateResources . resourcesElements . elementsEnergy . count) e
applyAction (SetStorageOfPaperclips a) =
  set (stateResources . resourcesStorage) a
applyAction (SetR r) =
  set (stateResearchAreas . advancedHelperResearch . researchCompProgress) r
applyAction (SetTreeSeeds s) =
  set (stateResources . resourcesElements . elementsTreeSeeds . count) s
applyAction (SetTrees t) =
  set (stateResources . resourcesElements . elementsTrees . count) t
applyAction (SetAdvancedHelperResearchProgress p) =
  set (stateResearchAreas . advancedHelperResearch . researchCompProgress) p
applyAction (SetHelperInc i) =
  set (stateConfig . configConstants . helperInc) i
applyAction (SetProgs ps) = set
  (stateResources . resourcesElements . elementsTreeSeeds . count . progs)
  ps
applyAction (SetWater w) =
  set (stateResources . resourcesElements . elementsWater . count) w
applyAction (SetWood w) =
  set (stateResources . resourcesElements . elementsWood . count) w


productOfHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Helpers a
productOfHelperWork (HelperInc inc) h = liftA2 (*) h inc

calcRemainingWater
  :: (Num a, Ord a) => CostWater a -> [Prog a] -> Water a -> Maybe (Water a)
calcRemainingWater price ps w =
  let calculatedCost = calcWaterCost ps (unWater $ view costWaterA price)
  in  case calculatedCost > w of
        True  -> Nothing
        False -> Just $ Iso.under2 isoWater (-) w calculatedCost

lineNeedMorePaperclips :: (Show a) => Seconds a -> ErrorLogLine
lineNeedMorePaperclips (Seconds s) = ErrorLogLine
  $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

mkErrorLogLine :: (Show a) => Seconds a -> Text -> ErrorLogLine
mkErrorLogLine (Seconds s) t =
  ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": ", t]

lineNeedMoreSeeds :: (Show a) => Seconds a -> ErrorLogLine
lineNeedMoreSeeds (Seconds s) = ErrorLogLine
  $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

-- When we initialize a seed, we take the first NotGrowing seed (if any) and
-- 'move it forward' to Growing or GrowingDone (depending on DurationTreeSeeds)
initializeASeed
  :: (Eq a, Num a) => DurationTreeSeeds a -> TreeSeeds a -> TreeSeeds a
initializeASeed dur =
  TreeSeeds
    . changeFirst (== NotGrowing)
                  (const $ moveItForward $ view durationTreeSeedsDur dur)
    . view progs
 where
  moveItForward Instant   = GrowingDone
  moveItForward (Ticks n) = Growing n

decPaperclipsWith'
  :: Num a => AdvancedHelperPrice (Paperclips a) -> Paperclips a -> Paperclips a
decPaperclipsWith' (AdvancedHelperPrice hp) p = liftA2 (-) p hp

removeLefts :: Zipper a -> Zipper a
removeLefts (Zip _ rs) = Zip [] rs

clearAllFutureSnapshots :: State a -> State a
clearAllFutureSnapshots = over (stateSnapshots . zipper) removeLefts

startResearch :: DurationAdvancedHelper a -> (ResearchProgress a)
startResearch = f . unDurationAdvanced
 where
  f Instant   = ResearchDone
  f (Ticks n) = ResearchInProgress n
