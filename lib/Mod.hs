module Mod where

import Data.Text (concat, pack, Text)
import Lens.Micro.Platform

data MyState = MyState
  { _actions :: [Action]
  , _errorLog :: [Text]
  , _paperclips :: Integer
  , _helpers :: Integer
  , _treeSeeds :: Integer
  , _seconds :: Integer
  , _isStarted :: Bool } deriving (Eq, Show)

actions :: Lens' MyState [Action]
actions f state = (\actions' -> state { _actions = actions'}) <$> f (_actions state)

viewActions :: MyState -> [Action]
viewActions = view actions

errorLog :: Lens' MyState [Text]
errorLog f state = (\errorLog' -> state { _errorLog = errorLog'}) <$> f (_errorLog state)

viewErrorLog :: MyState -> [Text]
viewErrorLog = view errorLog

paperclips :: Lens' MyState Integer
paperclips f state = (\paperclips' -> state { _paperclips = paperclips'}) <$> f (_paperclips state)

viewPaperclips :: MyState -> Integer
viewPaperclips = view paperclips

helpers :: Lens' MyState Integer
helpers f state = (\helpers' -> state { _helpers = helpers'}) <$> f (_helpers state)

viewHelpers :: MyState -> Integer
viewHelpers = view helpers

treeSeeds :: Lens' MyState Integer
treeSeeds f state = (\treeSeeds' -> state { _treeSeeds = treeSeeds'}) <$> f (_treeSeeds state)

viewTreeSeeds :: MyState -> Integer
viewTreeSeeds = view treeSeeds

seconds :: Lens' MyState Integer
seconds f state = (\seconds' -> state { _seconds = seconds'}) <$> f (_seconds state)

viewSeconds :: MyState -> Integer
viewSeconds = view seconds

isStarted :: Lens' MyState Bool
isStarted f state = (\isStarted' -> state { _isStarted = isStarted'}) <$> f (_isStarted state)

viewIsStarted :: MyState -> Bool
viewIsStarted = view isStarted

data MyEvent
  = Start
  | CreatePC
  | CreateHelper
  | PlantASeed
  | ExitApplication
  | Tick
  deriving (Eq, Show)

data Action = CreateHelperAction deriving (Eq, Show)
