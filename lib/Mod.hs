{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mod where

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
  arbitrary = do
    r <- Test.Tasty.QuickCheck.elements
      [ SetP undefined
      , SetH undefined
      , SetE undefined
      , SetR undefined
      , SetTreeSeeds undefined
      , SetTrees undefined
      , SetAdvancedHelperResearchProgress undefined
      , SetHelperInc undefined
      , SetProgs undefined]
    case r of
      SetP _ -> SetP <$> arbitrary
      SetH _ -> SetH <$> arbitrary
      SetE _ -> SetE <$> arbitrary
      SetR _ -> SetR <$> arbitrary
      SetTreeSeeds _ -> SetTreeSeeds <$> arbitrary
      SetTrees _ -> SetTrees <$> arbitrary
      SetAdvancedHelperResearchProgress _ -> SetAdvancedHelperResearchProgress <$> arbitrary
      SetHelperInc _ -> SetHelperInc <$> arbitrary
      SetProgs _ -> arbitrary

instance Arbitrary ErrorLogLine where
  arbitrary = ErrorLogLine <$> arbitrary

instance Arbitrary Resources where
  arbitrary = Resources
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Paperclips where
  arbitrary = Paperclips . getNonNegative <$> arbitrary

instance Arbitrary Storage where
  arbitrary = Storage . getNonNegative <$> arbitrary

instance Arbitrary Trees where
  arbitrary = Trees . getNonNegative <$> arbitrary

instance Arbitrary TreeSeeds where
  arbitrary = elements []

instance Arbitrary Wood where
  arbitrary = Wood . getNonNegative <$> arbitrary

instance Arbitrary Seconds where
  arbitrary = Seconds . getNonNegative <$> arbitrary

instance Arbitrary ResearchAreas where
  arbitrary = ResearchAreas <$> arbitrary

instance Arbitrary ResearchComp where
  arbitrary = ResearchComp <$> arbitrary <*> arbitrary

instance Arbitrary Duration where
  arbitrary = Duration . getNonNegative <$> arbitrary

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

newtype TreePrice = TreePrice { unTreePrice :: Integer } deriving (Eq, Show)

data Action
  = SetP Paperclips
  | SetH Helpers
  | SetE ErrorLogLine
  | SetR ResearchProgress
  | SetTreeSeeds TreeSeeds
  | SetTrees Trees
  | SetAdvancedHelperResearchProgress ResearchProgress
  | SetHelperInc HelperInc
  | SetProgs [Prog]
  deriving (Eq, Show)

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text } deriving (Eq)

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

data Resources = Resources
  { _paperclips :: Paperclips
  , _helpers :: Helpers
  , _storage :: Storage
  , _trees :: Trees
  , _treeSeeds :: TreeSeeds
  , _wood :: Wood } deriving (Eq, Show)

newtype Paperclips = Paperclips { unPaperclips :: Integer } deriving (Enum, Eq, Ord)

instance Show Paperclips where
  show (Paperclips a) = show a

newtype Helpers = Helpers { unHelpers :: Integer } deriving (Enum, Eq, Ord)

instance Show Helpers where
  show (Helpers a) = show a

newtype Storage = Storage { unStorage :: Integer } deriving (Eq)

instance Show Storage where
  show (Storage a) = show a

newtype Trees = Trees { unTrees :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Trees where
  show (Trees a) = show a

newtype TreeSeeds = TreeSeeds { unTreeSeeds :: [Prog]} deriving (Eq)

instance Show TreeSeeds where
  show (TreeSeeds a) = show a

data Prog = NotGrowing | Growing Int | GrowingDone deriving (Eq)

instance Show Prog where
  show NotGrowing = show "Not growing"
  show (Growing n) = show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
    where
      noun 1 = "tick"
      noun _ = "ticks"
  show GrowingDone = show "Growing done"

newtype Wood = Wood { unWood :: Integer } deriving (Enum, Eq, Num, Ord)

instance Show Wood where
  show (Wood a) = show a

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
  | ResearchAdvancedHelper
  | PlantASeed
  | ExitApplication
  | Tick
  deriving (Eq, Show)

startResearch :: Duration -> ResearchProgress
startResearch (Duration n) = ResearchInProgress n