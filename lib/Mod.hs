{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mod where

import Data.Text (Text)

import Resources

data MyState = MyState
  { _config :: Config
  , _actions :: [Action]
  , _errorLog :: [ErrorLogLine]
  , _researchAreas :: ResearchAreas
  , _resources :: Resources
  , _seconds :: Seconds
  , _isStarted :: IsStarted } deriving (Eq, Show)

data Config = Config
  { _constants :: Constants
  , _durations :: Durations
  , _prices :: Prices } deriving (Eq, Show)

data Durations = Durations
  { _treeDuration :: TreeDuration } deriving (Eq, Show)

newtype TreeDuration = TreeDuration { unTreeDuration :: Integer } deriving (Eq, Show)

data Constants = Constants
  { _helperInc :: HelperInc } deriving (Eq, Show)

newtype HelperInc = HelperInc { unHelperInc :: Helpers } deriving (Eq, Show)

data Prices = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice
  , _helperPrice :: HelperPrice
  , _progPrice :: ProgPrice
  , _treePrice :: TreePrice
  , _treeSeedPrice :: TreeSeedPrice } deriving (Eq, Show)

newtype AdvancedHelperPrice = AdvancedHelperPrice { unAdvancedHelperPrice :: Paperclips } deriving (Eq, Show)

newtype HelperPrice = HelperPrice { unHelperPrice :: Paperclips } deriving (Eq, Show)

newtype ProgPrice = ProgPrice { unProgPrice :: Integer } deriving (Eq, Show)

newtype TreePrice = TreePrice { unTreePrice :: Integer } deriving (Eq, Show)

newtype TreeSeedPrice = TreeSeedPrice { unTreeSeedPrice :: Paperclips } deriving (Eq, Show)

data Action
  = SetP Paperclips
  | SetH Helpers
  | SetE ErrorLogLine
  | SetR ResearchProgress
  | SetTreeSeeds TreeSeeds
  | SetTrees Trees
  | SetWater Water
  | SetAdvancedHelperResearchProgress ResearchProgress
  | SetHelperInc HelperInc
  | SetProgs [Prog]
  deriving (Eq, Show)

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text } deriving (Eq)

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

data ResearchProgress = NotResearched | ResearchInProgress Integer | ResearchDone
  deriving (Eq)

instance Show ResearchProgress where
  show NotResearched = "Not researched"
  show (ResearchInProgress n) = "Research in progress - " ++ show n ++ " " ++ noun n ++ " left."
    where
      noun 1 = "tick"
      noun _ = "ticks"
  show ResearchDone = "Research done"

data ResearchAreas = ResearchAreas
  { _advancedHelperResearch :: ResearchComp } deriving Eq

instance Show ResearchAreas where
  show (ResearchAreas a) = show a

data ResearchComp = ResearchComp
  { _researchCompDuration :: Duration
  , _researchCompProgress :: ResearchProgress } deriving (Eq, Show)

newtype Duration = Duration { unDuration :: Integer } deriving (Eq, Show)

newtype Seconds = Seconds { unSeconds :: Integer } deriving (Enum, Eq, Num)

instance Show Seconds where
  show (Seconds a) = show a

newtype IsStarted = IsStarted { unIsStarted :: Bool } deriving (Eq)

instance Show IsStarted where
  show (IsStarted a) = show a


data MyEvent
  = Start
  | CreatePaperclip
  | CreateHelper
  | PumpWater
  | PlantASeed
  | BuyASeed
  | ResearchAdvancedHelper
  | ExitApplication
  | Tick
  deriving (Eq, Show)

startResearch :: Duration -> ResearchProgress
startResearch (Duration n) = ResearchInProgress n