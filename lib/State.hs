{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Data.Text                      ( Text
                                                , concat
                                                , pack
                                                , unpack
                                                )

import           Config
import           Elements
import           Iso
import           NaturalTransformation
import           Resources
import           Seconds
import           Source
import           Utils

data Action a
  = SetP (Paperclips a)
  | SetH (Helpers a)
  | SetE ErrorLogLine
  | SetEnergy (Energy a)
  | SetR (ResearchProgress a)
  | SetTreeSeeds (TreeSeeds a)
  | SetTrees (Trees a)
  | SetWater (Water a)
  | SetAdvancedHelperResearchProgress (ResearchProgress a)
  | SetHelperInc (HelperInc (Helpers a))
  | SetProgs [Prog a]

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
  , _researchCompProgress :: ResearchProgress a }
makeLenses ''ResearchComp

data ResearchAreas a = ResearchAreas
  { _advancedHelperResearch :: ResearchComp a}
makeLenses ''ResearchAreas

newtype IsStarted = IsStarted { unIsStarted :: Bool }

instance Show IsStarted where
  show (IsStarted a) = show a

data Status = Enabled | Disabled

data MyEvent
  = Start
  | CreatePaperclip
  | CreateHelper
  | GenerateEnergy
  | PumpWater
  | PlantASeed
  | BuyASeed
  | ResearchAdvancedHelper
  | ExitApplication
  | Tick
  | Compile Text
  deriving (Eq, Show)

newtype ButtonTitle = ButtonTitle Text

instance Show ButtonTitle where
  show (ButtonTitle t) = unpack t

newtype ButtonStatus = ButtonStatus Status

newtype ButtonEvent = ButtonEvent MyEvent

data ButtonData = ButtonData
  { _buttonTitle :: ButtonTitle
  , _buttonStatus :: ButtonStatus
  , _buttonEvent :: ButtonEvent }
makeLenses ''ButtonData

newtype EventStart =
  EventStart { _eventStartButtonData :: ButtonData }
makeLenses ''EventStart

newtype EventCreatePaperclip =
  EventCreatePaperclip { _eventCreatePaperclipButtonData :: ButtonData }
makeLenses ''EventCreatePaperclip

newtype EventCreateHelper =
  EventCreateHelper { _eventCreateHelperButtonData :: ButtonData }
makeLenses ''EventCreateHelper

newtype EventPumpWater =
  EventPumpWater { _eventPumpWaterButtonData :: ButtonData }
makeLenses ''EventPumpWater

newtype EventGenerateEnergy =
  EventGenerateEnergy { _eventGenerateEnergyButtonData :: ButtonData }
makeLenses ''EventGenerateEnergy

newtype EventBuyASeed =
  EventBuyASeed { _eventBuyASeedButtonData :: ButtonData }
makeLenses ''EventBuyASeed

newtype EventPlantASeed =
  EventPlantASeed { _eventPlantASeedButtonData :: ButtonData }
makeLenses ''EventPlantASeed

newtype EventResearchAdvancedHelper =
  EventResearchAdvancedHelper { _eventResearchAdvancedHelperButtonData :: ButtonData }
makeLenses ''EventResearchAdvancedHelper

newtype EventExitApplication =
  EventExitApplication { _eventExitApplicationButtonData :: ButtonData }
makeLenses ''EventExitApplication

data Events = Events
  { _eventStart :: EventStart
  , _eventCreatePaperclip :: EventCreatePaperclip
  , _eventCreateHelper :: EventCreateHelper
  , _eventPumpWater :: EventPumpWater
  , _eventGenerateEnergy :: EventGenerateEnergy
  , _eventBuyASeed :: EventBuyASeed
  , _eventPlantASeed :: EventPlantASeed
  , _eventResearchAdvancedHelper :: EventResearchAdvancedHelper
  , _eventExitApplication :: EventExitApplication }
makeLenses ''Events

data Button
  = ButtonStart
  | ButtonCreatePaperclip
  | ButtonCreateHelper
  | ButtonPumpWater
  | ButtonGenerateEnergy
  | ButtonBuyASeed
  | ButtonPlantASeed
  | ButtonResearchAdvancedHelper
  | ButtonExitApplication

newtype Title = Title Text

instance Show Title where
  show (Title t) = unpack t

data State a = State
  { _config :: Config a
  , _actions :: [Action a]
  , _errorLog :: [ErrorLogLine]
  , _events :: Events
  , _researchAreas :: ResearchAreas a
  , _resources :: Resources a
  , _seconds :: Seconds a
  , _source :: Source a
  , _title :: Title
  , _isStarted :: IsStarted }
makeLenses ''State

applyAction :: Num a => Action a -> State a -> State a
applyAction (SetP p) state =
  set (resources . elements . elementPaperclips . count) p state
applyAction (SetH h) state =
  set (resources . elements . elementHelpers . count) h state
applyAction (SetE err) state = over errorLog (\errs -> err : errs) state
applyAction (SetEnergy e) state =
  set (resources . elements . elementEnergy . count) e state
applyAction (SetR r) state =
  set (researchAreas . advancedHelperResearch . researchCompProgress) r state
applyAction (SetTreeSeeds s) state =
  set (resources . elements . elementTreeSeeds . count) s state
applyAction (SetTrees t) state =
  set (resources . elements . elementTrees . count) t state
applyAction (SetAdvancedHelperResearchProgress p) state =
  set (researchAreas . advancedHelperResearch . researchCompProgress) p state
applyAction (SetHelperInc i) state =
  set (config . constants . helperInc) i state
applyAction (SetProgs ps) state =
  set (resources . elements . elementTreeSeeds . count . progs) ps state
applyAction (SetWater w) state =
  set (resources . elements . elementWater . count) w state

addHelperWork
  :: Num a => HelperInc (Helpers a) -> Helpers a -> Paperclips a -> Paperclips a
addHelperWork inc h p =
  liftA2 (+) p $ unNat helpersToPaperclips $ productOfHelperWork inc h

productOfHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Helpers a
productOfHelperWork (HelperInc inc) h = liftA2 (*) h inc

calcRemainingWater
  :: (Num a, Ord a)
  => TreeSeedCostPerTick a
  -> [Prog a]
  -> Water a
  -> Maybe (Water a)
calcRemainingWater price ps w =
  let calculatedCost = calcWaterCost
        ps
        (unWater $ view costWater $ unTreeSeedCostPerTick price)
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

concatErrors :: (Show a) => Seconds a -> ErrorCount PaymentError -> ErrorLogLine
concatErrors s errorCount = case errorCount of
  OneError e -> mkErrorLogLine s (paymentErrorToText e)
  TwoErrors e1 e2 ->
    mkErrorLogLine s (paymentErrorToText e1 <> " " <> paymentErrorToText e2)

-- When we initialize a seed, we take the first NotGrowing seed (if any) and
-- 'move it forward' to Growing or GrowingDone (depending on DurationTreeSeeds)
initializeASeed
  :: (Eq a, Num a) => DurationTreeSeeds a -> TreeSeeds a -> TreeSeeds a
initializeASeed dur =
  TreeSeeds
    . changeFirst (== NotGrowing)
                  (const $ moveItForward $ view durationTreeSeeds dur)
    . view treeSeeds
 where
  moveItForward Instant   = GrowingDone
  moveItForward (Ticks n) = Growing n

decPaperclipsWith'
  :: Num a => AdvancedHelperPrice (Paperclips a) -> Paperclips a -> Paperclips a
decPaperclipsWith' (AdvancedHelperPrice hp) p = liftA2 (-) p hp

startResearch :: DurationAdvancedHelper a -> (ResearchProgress a)
startResearch = f . unDurationAdvanced
 where
  f Instant   = ResearchDone
  f (Ticks n) = ResearchInProgress n
