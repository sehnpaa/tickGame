{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Lens'
                                                , Profunctor
                                                , iso
                                                , makeLenses
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

data ResearchAreas a = ResearchAreas
  { _advancedHelperResearch :: ResearchComp a}

data ResearchComp a = ResearchComp
  { _researchCompDuration :: DurationAdvancedHelper a
  , _researchCompProgress :: ResearchProgress a }

newtype DurationAdvancedHelper a = DurationAdvanced { unDurationAdvanced :: Duration a }

newtype Seconds a = Seconds { unSeconds :: a } deriving (Enum, Eq)

instance Show (Seconds Integer) where
  show (Seconds a) = show a

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

data ButtonData = ButtonData { _buttonData :: (Text, Status, MyEvent) }
makeLenses ''ButtonData

newtype EventStart = EventStart { _eventStartButtonData :: ButtonData }
makeLenses ''EventStart

newtype EventCreatePaperclip = EventCreatePaperclip { _eventCreatePaperclipButtonData :: ButtonData }
makeLenses ''EventCreatePaperclip

newtype EventCreateHelper = EventCreateHelper { _eventCreateHelperButtonData :: ButtonData }
makeLenses ''EventCreateHelper

newtype EventPumpWater = EventPumpWater { _eventPumpWaterButtonData :: ButtonData }
makeLenses ''EventPumpWater

newtype EventGenerateEnergy = EventGenerateEnergy { _eventGenerateEnergyButtonData :: ButtonData }
makeLenses ''EventGenerateEnergy

newtype EventBuyASeed = EventBuyASeed { _eventBuyASeedButtonData :: ButtonData }
makeLenses ''EventBuyASeed

newtype EventPlantASeed = EventPlantASeed { _eventPlantASeedButtonData :: ButtonData }
makeLenses ''EventPlantASeed

newtype EventResearchAdvancedHelper = EventResearchAdvancedHelper { _eventResearchAdvancedHelperButtonData :: ButtonData }
makeLenses ''EventResearchAdvancedHelper

newtype EventExitApplication = EventExitApplication { _eventExitApplicationButtonData :: ButtonData }
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

data Button = ButtonStart | ButtonCreatePaperclip | ButtonCreateHelper | ButtonPumpWater |
  ButtonGenerateEnergy |
  ButtonBuyASeed | ButtonPlantASeed | ButtonResearchAdvancedHelper
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


applyAction :: Action a -> State a -> State a
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
productOfHelperWork inc h = liftA2 (*) h $ Iso.unwrap isoHelperInc inc

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

initializeASeed
  :: (Eq a, Num a) => DurationTreeSeeds a -> TreeSeeds a -> TreeSeeds a
initializeASeed dur =
  TreeSeeds
    . changeFirst (== NotGrowing) (const $ f $ view durationTreeSeeds dur)
    . view treeSeeds
 where
  f Instant   = GrowingDone
  f (Ticks n) = Growing n

decPaperclipsWith'
  :: Num a => AdvancedHelperPrice (Paperclips a) -> Paperclips a -> Paperclips a
decPaperclipsWith' (AdvancedHelperPrice hp) p = liftA2 (-) p hp

---

helperInc :: Lens' (Constants a) (HelperInc (Helpers a))
helperInc f state =
  (\inc' -> state { _helperInc = inc' }) <$> f (_helperInc state)

researchCompProgress :: Lens' (ResearchComp a) (ResearchProgress a)
researchCompProgress f state =
  (\progress' -> state { _researchCompProgress = progress' })
    <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' (ResearchAreas a) (ResearchComp a)
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a })
  <$> f (_advancedHelperResearch state)

researchCompDuration :: Lens' (ResearchComp a) (DurationAdvancedHelper a)
researchCompDuration f state = (\dur -> state { _researchCompDuration = dur })
  <$> f (_researchCompDuration state)



isoAdvancedHelperPrice
  :: (Profunctor p, Functor f)
  => p
       (AdvancedHelperPrice (Paperclips a))
       (f (AdvancedHelperPrice (Paperclips a)))
  -> p (Paperclips a) (f (Paperclips a))
isoAdvancedHelperPrice = iso AdvancedHelperPrice unAdvancedHelperPrice

isoHelperInc
  :: (Profunctor p, Functor f)
  => p (HelperInc (b a)) (f (HelperInc (b a)))
  -> p (b a) (f (b a))
isoHelperInc = iso HelperInc unHelperInc

isoHelperPrice
  :: (Profunctor p, Functor f)
  => p (HelperPrice a) (f (HelperPrice a))
  -> p (Paperclips a) (f (Paperclips a))
isoHelperPrice = iso HelperPrice unHelperPrice

startResearch :: DurationAdvancedHelper a -> (ResearchProgress a)
startResearch = f . unDurationAdvanced
 where
  f Instant   = ResearchDone
  f (Ticks n) = ResearchInProgress n
