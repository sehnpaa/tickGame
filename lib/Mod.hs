{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Mod where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Lens'
                                                , Profunctor
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
  { _researchCompDuration :: DurationAdvancedHelper a
  , _researchCompProgress :: ResearchProgress }

newtype DurationAdvancedHelper a = DurationAdvanced { unDurationAdvanced :: Duration a }

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
  let cost = calcWaterCost progs (unProgPrice price)
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

initializeSeed
  :: (Eq a, Num a) => DurationTreeSeeds a -> TreeSeeds a -> TreeSeeds a
initializeSeed duration =
  TreeSeeds
    . changeFirst (== NotGrowing) (const $ f $ view durationTreeSeeds duration)
    . unTreeSeeds
 where
  f Instant   = GrowingDone
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

config :: Lens' MyState Config
config f state =
  (\config' -> state { _config = config' }) <$> f (_config state)

helperInc :: Lens' (Constants a) (HelperInc (Helpers a))
helperInc f state =
    (\inc' -> state { _helperInc = inc' }) <$> f (_helperInc state)

actions :: Lens' MyState [Action]
actions f state =
    (\actions' -> state { _actions = actions' }) <$> f (_actions state)

errorLog :: Lens' MyState [ErrorLogLine]
errorLog f state =
    (\errorLog' -> state { _errorLog = errorLog' }) <$> f (_errorLog state)

resources :: Lens' MyState Resources
resources f state =
    (\resources' -> state { _resources = resources' }) <$> f (_resources state)

researchAreas :: Lens' MyState ResearchAreas
researchAreas f state =
    (\areas' -> state { _researchAreas = areas' }) <$> f (_researchAreas state)

seconds :: Lens' MyState Seconds
seconds f state =
    (\seconds' -> state { _seconds = seconds' }) <$> f (_seconds state)

isStarted :: Lens' MyState IsStarted
isStarted f state =
    (\isStarted' -> state { _isStarted = isStarted' }) <$> f (_isStarted state)

researchCompProgress :: Lens' (ResearchComp Integer) ResearchProgress
researchCompProgress f state =
    (\progress' -> state { _researchCompProgress = progress' })
        <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' ResearchAreas (ResearchComp Integer)
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a })
    <$> f (_advancedHelperResearch state)

researchCompDuration
    :: Lens' (ResearchComp a) (DurationAdvancedHelper a)
researchCompDuration f state =
    undefined (\duration -> state { _researchCompDuration = duration })
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

startResearch :: DurationAdvancedHelper Integer -> ResearchProgress
startResearch = f . unDurationAdvanced
 where
  f Instant   = ResearchDone
  f (Ticks n) = ResearchInProgress n
