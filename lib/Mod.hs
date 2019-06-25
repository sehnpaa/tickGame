{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Mod where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Profunctor
                                                , iso
                                                , view
                                                , withIso
                                                )
import           Data.Text                      ( Text
                                                , concat
                                                , pack
                                                )

import           Config
import           Elements
import           Iso
import           NaturalTransformation
import           Resources
import           Utils

data MyState = MyState
  { _config :: Config
  , _actions :: [Action]
  , _errorLog :: [ErrorLogLine]
  , _researchAreas :: ResearchAreas
  , _resources :: Resources
  , _seconds :: Seconds
  , _isStarted :: IsStarted }

data Action
  = SetP (Paperclips Integer)
  | SetH (Helpers Integer)
  | SetE ErrorLogLine
  | SetR ResearchProgress
  | SetTreeSeeds (TreeSeeds Integer)
  | SetTrees (Trees Integer)
  | SetWater (Water Integer)
  | SetAdvancedHelperResearchProgress ResearchProgress
  | SetHelperInc (HelperInc (Helpers Integer))
  | SetProgs [Prog Integer]

newtype ErrorLogLine = ErrorLogLine { unErrorLogLine :: Text }

instance Show ErrorLogLine where
  show (ErrorLogLine a) = show a

data ResearchProgress = NotResearched | ResearchInProgress Integer | ResearchDone
  deriving (Eq)

instance Show ResearchProgress where
  show NotResearched = "Not researched"
  show (ResearchInProgress n) =
    "Research in progress - " ++ show n ++ " " ++ noun n ++ " left."
   where
    noun 1 = "tick"
    noun _ = "ticks"
  show ResearchDone = "Research done"

data ResearchAreas = ResearchAreas
  { _advancedHelperResearch :: ResearchComp Integer}

data ResearchComp a = ResearchComp
  { _researchCompDuration :: DurationAdvancedHelper (Duration a)
  , _researchCompProgress :: ResearchProgress }

newtype DurationAdvancedHelper a = DurationAdvanced { unDurationAdvanced :: a }

newtype Seconds = Seconds { unSeconds :: Integer } deriving (Enum, Eq, Num)

instance Show Seconds where
  show (Seconds a) = show a

newtype IsStarted = IsStarted { unIsStarted :: Bool }

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

addHelperWork
  :: Num a => HelperInc (Helpers a) -> Helpers a -> Paperclips a -> Paperclips a
addHelperWork inc h p =
  liftA2 (+) p $ unNat helpersToPaperclips $ productOfHelperWork inc h

productOfHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Helpers a
productOfHelperWork inc h = liftA2 (*) h $ Iso.unwrap isoHelperInc inc

calcRemainingWater
  :: (Num a, Ord a) => ProgPrice a -> [Prog a] -> Water a -> Maybe (Water a)
calcRemainingWater price progs water =
  let cost = waterCost progs (unProgPrice price)
  in  case cost > water of
        True  -> Nothing
        False -> Just $ Iso.under2 isoWater (-) water cost

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine
  $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

mkErrorLogLine :: Seconds -> Text -> ErrorLogLine
mkErrorLogLine s t =
  ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": ", t]

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine
  $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

initializeSeed :: (Eq a, Num a) => DurationTreeSeeds (Duration a) -> TreeSeeds a -> TreeSeeds a
initializeSeed duration =
  TreeSeeds
    . changeFirst (== NotGrowing)
                  (const $ f $ unDurationTreeSeeds duration)
    . unTreeSeeds
    where
      f Instant = GrowingDone
      f (Ticks n) = Growing n

decPaperclipsWith :: Num a => Cost a -> Paperclips a -> Paperclips a
decPaperclipsWith c p = under2 isoPaperclips (-) p $ view paperclipCost c

decPaperclipsWith'
  :: Num a => AdvancedHelperPrice (Paperclips a) -> Paperclips a -> Paperclips a
decPaperclipsWith' hp p = withIso
  isoAdvancedHelperPrice
  (\_ eli price -> Iso.under2 isoPaperclips (-) p (eli price))
  hp

---

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

startResearch :: DurationAdvancedHelper (Duration Integer) -> ResearchProgress
startResearch = f . unDurationAdvanced
      where
        f Instant = ResearchDone
        f (Ticks n) = ResearchInProgress n
