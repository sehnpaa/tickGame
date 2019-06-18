{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mod where

import Control.Applicative (liftA2)
import Control.Lens (Profunctor, iso)
import Data.Text (Text)

import Iso
import NaturalTransformation
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
  { _helperInc :: HelperInc (Helpers Integer) } deriving (Eq, Show)

instance Show (HelperInc (Helpers Integer)) where
  show (HelperInc a) = show a

newtype HelperInc a = HelperInc { unHelperInc :: a} deriving (Eq, Functor)

instance Applicative HelperInc where
  pure = HelperInc
  HelperInc f <*> HelperInc a = HelperInc (f a)

data Prices = Prices
  { _advancedHelperPrice :: AdvancedHelperPrice (Paperclips Integer)
  , _helperPrice :: HelperPrice Integer
  , _progPrice :: ProgPrice
  , _treePrice :: TreePrice
  , _treeSeedPrice :: TreeSeedPrice } deriving (Eq, Show)

newtype AdvancedHelperPrice a = AdvancedHelperPrice { unAdvancedHelperPrice :: a } deriving (Eq)

instance Show (AdvancedHelperPrice (Paperclips Integer)) where
  show (AdvancedHelperPrice a) = show a

newtype HelperPrice a = HelperPrice { unHelperPrice :: Paperclips a } deriving (Eq, Functor)

instance Show (HelperPrice Integer) where
  show (HelperPrice a) = show a

newtype ProgPrice = ProgPrice { unProgPrice :: Integer } deriving (Eq, Show)

newtype TreePrice = TreePrice { unTreePrice :: Integer } deriving (Eq, Show)

newtype TreeSeedPrice = TreeSeedPrice { unTreeSeedPrice :: Paperclips Integer } deriving (Eq, Show)

data Action
  = SetP (Paperclips Integer)
  | SetH (Helpers Integer)
  | SetE ErrorLogLine
  | SetR ResearchProgress
  | SetTreeSeeds TreeSeeds
  | SetTrees Trees
  | SetWater Water
  | SetAdvancedHelperResearchProgress ResearchProgress
  | SetHelperInc (HelperInc (Helpers Integer))
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

---

helperWork2 :: (Num a, Ord a) => Paperclips a -> Helpers a -> HelperInc (Helpers a) -> Storage (Paperclips a) -> Paperclips a
helperWork2 p h inc storage = limitByStorage storage $ addHelperWork inc h p

addHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Paperclips a -> Paperclips a
addHelperWork inc h p = liftA2 (+) p $ unNat helpersToPaperclips $ productOfHelperWork inc h

productOfHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Helpers a
productOfHelperWork inc h = liftA2 (*) h $ Iso.unwrap isoHelperInc inc

---

isoAdvancedHelperPrice :: (Profunctor p, Functor f) => p (AdvancedHelperPrice (Paperclips a)) (f (AdvancedHelperPrice (Paperclips a))) -> p (Paperclips a) (f (Paperclips a))
isoAdvancedHelperPrice = iso AdvancedHelperPrice unAdvancedHelperPrice

isoHelperInc :: (Profunctor p, Functor f) => p (HelperInc (b a)) (f (HelperInc (b a))) -> p (b a) (f (b a))
isoHelperInc = iso HelperInc unHelperInc

isoHelperPrice :: (Profunctor p, Functor f) => p (HelperPrice a) (f (HelperPrice a)) -> p (Paperclips a) (f (Paperclips a))
isoHelperPrice = iso HelperPrice unHelperPrice

isoTreePrice :: (Profunctor p, Functor f) => p TreePrice (f TreePrice) -> p Integer (f Integer)
isoTreePrice = iso TreePrice unTreePrice

startResearch :: Duration -> ResearchProgress
startResearch (Duration n) = ResearchInProgress n