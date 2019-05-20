{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mod where

import Control.Lens
import Data.Text (pack, Text)
import Test.Tasty.QuickCheck

data MyState = MyState
  { _config :: Config
  , _actions :: [Action]
  , _errorLog :: [ErrorLogLine]
  , _researchAreas :: ResearchAreas
  , _resources :: Resources
  , _seconds :: Seconds
  , _isStarted :: IsStarted } deriving (Eq, Show)

instance Arbitrary MyState where
  arbitrary = MyState <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary <*> arbitrary

instance Arbitrary Constants where
  arbitrary = Constants <$> arbitrary

instance Arbitrary HelperInc where
  arbitrary = HelperInc <$> arbitrary

instance Arbitrary Prices where
  arbitrary = Prices <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary AdvancedHelperPrice where
  arbitrary = AdvancedHelperPrice <$> arbitrary

instance Arbitrary HelperPrice where
  arbitrary = HelperPrice <$> arbitrary

instance Arbitrary TreePrice where
  arbitrary = TreePrice <$> arbitrary

instance Arbitrary Helpers where
  arbitrary = Helpers <$> arbitrary

instance Arbitrary Action where
  arbitrary = Action <$> arbitrary

instance Arbitrary ErrorLogLine where
  arbitrary = ErrorLogLine <$> arbitrary

instance Arbitrary Resources where
  arbitrary = Resources <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Paperclips where
  arbitrary = Paperclips <$> arbitrary

instance Arbitrary Storage where
  arbitrary = Storage <$> arbitrary

instance Arbitrary Trees where
  arbitrary = Trees <$> arbitrary

instance Arbitrary TreeSeeds where
  arbitrary = TreeSeeds <$> arbitrary

instance Arbitrary Seconds where
  arbitrary = Seconds <$> arbitrary

instance Arbitrary ResearchAreas where
  arbitrary = ResearchAreas <$> arbitrary

instance Arbitrary ResearchComp where
  arbitrary = ResearchComp <$> arbitrary <*> arbitrary

instance Arbitrary Duration where
  arbitrary = Duration <$> arbitrary

instance Arbitrary ResearchProgress where
  arbitrary = do
    r <- Test.Tasty.QuickCheck.elements [NotResearched, ResearchInProgress undefined, ResearchDone]
    case r of
      NotResearched -> return NotResearched
      ResearchInProgress _ -> ResearchInProgress <$> arbitrary
      ResearchDone -> return ResearchDone

instance Arbitrary IsStarted where
  arbitrary = IsStarted <$> arbitrary

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

data Config = Config
  { _constants :: Constants
  , _prices :: Prices } deriving (Eq, Show)

data Constants = Constants
  { _helperInc :: HelperInc } deriving (Eq, Show)

newtype HelperInc = HelperInc { unHelperInc :: Helpers } deriving (Eq, Show)

data Prices = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice
  , _helperPrice :: HelperPrice
  , _treePrice :: TreePrice } deriving (Eq, Show)

newtype AdvancedHelperPrice = AdvancedHelperPrice { unAdvancedHelperPrice :: Paperclips } deriving (Eq, Show)

newtype HelperPrice = HelperPrice { unHelperPrice :: Paperclips } deriving (Eq, Show)

newtype TreePrice = TreePrice { unTreePrice :: TreeSeeds } deriving (Eq, Show)

newtype Action = Action { unAction :: ()} deriving (Eq, Show)

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text } deriving (Eq)

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

data Resources = Resources
  { _paperclips :: Paperclips
  , _helpers :: Helpers
  , _storage :: Storage
  , _trees :: Trees
  , _treeSeeds :: TreeSeeds } deriving (Eq, Show)

newtype Paperclips = Paperclips { unPaperclips :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Paperclips where
  show (Paperclips a) = show a

newtype Helpers = Helpers { unHelpers :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Helpers where
  show (Helpers a) = show a

newtype Storage = Storage { unStorage :: Integer } deriving (Eq)

instance Show Storage where
  show (Storage a) = show a

newtype Trees = Trees { unTrees :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Trees where
  show (Trees a) = show a

newtype TreeSeeds = TreeSeeds { unTreeSeeds :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show TreeSeeds where
  show (TreeSeeds a) = show a

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

config :: Lens' MyState Config
config f state = (\config' -> state { _config = config'}) <$> f (_config state)

constants :: Lens' Config Constants
constants f state = (\constants' -> state { _constants = constants'}) <$> f (_constants state)

helperInc :: Lens' Constants HelperInc
helperInc f state = (\inc' -> state { _helperInc = inc'}) <$> f (_helperInc state)

prices :: Lens' Config Prices
prices f state = (\prices' -> state { _prices = prices'}) <$> f (_prices state)

advancedHelperPrice :: Lens' Prices AdvancedHelperPrice
advancedHelperPrice f state = (\price' -> state { _advancedHelperPrice = price'}) <$> f (_advancedHelperPrice state)

helperPrices :: Lens' Prices HelperPrice
helperPrices f state = (\helperPrice' -> state { _helperPrice = helperPrice'}) <$> f (_helperPrice state)

treePrice :: Lens' Prices TreePrice
treePrice f state = (\treePrice' -> state { _treePrice = treePrice'}) <$> f (_treePrice state)

actions :: Lens' MyState [Action]
actions f state = (\actions' -> state { _actions = actions'}) <$> f (_actions state)

viewActions :: MyState -> [Action]
viewActions = view actions

errorLog :: Lens' MyState [ErrorLogLine]
errorLog f state = (\errorLog' -> state { _errorLog = errorLog'}) <$> f (_errorLog state)

resources :: Lens' MyState Resources
resources f state = (\resources' -> state { _resources = resources'}) <$> f (_resources state)

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

paperclips :: Lens' Resources Paperclips
paperclips f state = (\paperclips' -> state { _paperclips = paperclips'}) <$> f (_paperclips state)

viewPaperclips :: MyState -> Paperclips
viewPaperclips = view (resources.paperclips)

helpers :: Lens' Resources Helpers
helpers f state = (\helpers' -> state { _helpers = helpers'}) <$> f (_helpers state)

viewHelpers :: MyState -> Helpers
viewHelpers = view (resources.helpers)

storage :: Lens' Resources Storage
storage f state = (\storage' -> state { _storage = storage'}) <$> f (_storage state)

viewStorage :: MyState -> Storage
viewStorage = view (resources.storage)

trees :: Lens' Resources Trees
trees f state = (\trees' -> state { _trees = trees'}) <$> f (_trees state)

viewTrees :: MyState -> Trees
viewTrees = view (resources.trees)

treeSeeds :: Lens' Resources TreeSeeds
treeSeeds f state = (\treeSeeds' -> state { _treeSeeds = treeSeeds'}) <$> f (_treeSeeds state)

viewTreeSeeds :: MyState -> TreeSeeds
viewTreeSeeds = view (resources.treeSeeds)

researchAreas :: Lens' MyState ResearchAreas
researchAreas f state = (\areas' -> state { _researchAreas = areas'}) <$> f (_researchAreas state)

researchCompProgress :: Lens' ResearchComp ResearchProgress
researchCompProgress f state = (\progress' -> state { _researchCompProgress = progress'}) <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' ResearchAreas ResearchComp
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a}) <$> f (_advancedHelperResearch state)

researchCompDuration :: Lens' ResearchComp Duration
researchCompDuration f state = (\duration -> state { _researchCompDuration = duration}) <$> f (_researchCompDuration state)

viewAdvancedHelperResearch :: MyState -> ResearchProgress
viewAdvancedHelperResearch = view (researchAreas.advancedHelperResearch.researchCompProgress)

seconds :: Lens' MyState Seconds
seconds f state = (\seconds' -> state { _seconds = seconds'}) <$> f (_seconds state)

viewSeconds :: MyState -> Seconds
viewSeconds = view seconds

isStarted :: Lens' MyState IsStarted
isStarted f state = (\isStarted' -> state { _isStarted = isStarted'}) <$> f (_isStarted state)

viewIsStarted :: MyState -> IsStarted
viewIsStarted = view isStarted

data MyEvent
  = Start
  | CreatePC
  | CreateHelper
  | ResearchAdvancedHelper
  | PlantASeed
  | ExitApplication
  | Tick
  deriving (Eq, Show)

startResearch :: Duration -> ResearchProgress
startResearch (Duration n) = ResearchInProgress n