{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mod where

import Data.Text (Text)
import Lens.Micro.Platform

data MyState = MyState
  { _config :: Config
  , _actions :: [Action]
  , _errorLog :: [ErrorLogLine]
  , _paperclips :: Paperclips
  , _helpers :: Helpers
  , _treeSeeds :: TreeSeeds
  , _seconds :: Seconds
  , _isStarted :: IsStarted } deriving (Eq, Show)

data Config = Config
  { _prices :: Prices } deriving (Eq, Show)

data Prices = Prices
  { _helperPrice :: HelperPrice } deriving (Eq, Show)

type HelperPrice = Paperclips

data Action = CreateHelperAction deriving (Eq, Show)

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text } deriving (Eq)

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

newtype Paperclips = Paperclips { unPaperclips :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Paperclips where
  show (Paperclips a) = show a

newtype Helpers = Helpers { unHelpers :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Helpers where
  show (Helpers a) = show a

newtype TreeSeeds = TreeSeeds { unTreeSeeds :: Integer } deriving (Enum, Eq, Num)

instance Show TreeSeeds where
  show (TreeSeeds a) = show a

newtype Seconds = Seconds { unSeconds :: Integer } deriving (Enum, Eq, Num)

instance Show Seconds where
  show (Seconds a) = show a

newtype IsStarted = IsStarted { unIsStarted :: Bool } deriving (Eq)

instance Show IsStarted where
  show (IsStarted a) = show a

config :: Lens' MyState Config
config f state = (\config' -> state { _config = config'}) <$> f (_config state)

prices :: Lens' Config Prices
prices f state = (\prices' -> state { _prices = prices'}) <$> f (_prices state)

helperPrices :: Lens' Prices Paperclips
helperPrices f state = (\helperPrice' -> state { _helperPrice = helperPrice'}) <$> f (_helperPrice state)

actions :: Lens' MyState [Action]
actions f state = (\actions' -> state { _actions = actions'}) <$> f (_actions state)

viewActions :: MyState -> [Action]
viewActions = view actions

errorLog :: Lens' MyState [ErrorLogLine]
errorLog f state = (\errorLog' -> state { _errorLog = errorLog'}) <$> f (_errorLog state)

viewErrorLog :: MyState -> [ErrorLogLine]
viewErrorLog = view errorLog

paperclips :: Lens' MyState Paperclips
paperclips f state = (\paperclips' -> state { _paperclips = paperclips'}) <$> f (_paperclips state)

viewPaperclips :: MyState -> Paperclips
viewPaperclips = view paperclips

helpers :: Lens' MyState Helpers
helpers f state = (\helpers' -> state { _helpers = helpers'}) <$> f (_helpers state)

viewHelpers :: MyState -> Helpers
viewHelpers = view helpers

treeSeeds :: Lens' MyState TreeSeeds
treeSeeds f state = (\treeSeeds' -> state { _treeSeeds = treeSeeds'}) <$> f (_treeSeeds state)

viewTreeSeeds :: MyState -> TreeSeeds
viewTreeSeeds = view treeSeeds

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
  | PlantASeed
  | ExitApplication
  | Tick
  deriving (Eq, Show)
