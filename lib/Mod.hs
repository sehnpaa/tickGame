{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Mod where

import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( Lens'
                                                , Profunctor
                                                , iso
                                                , over
                                                , set
                                                , view
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

data MyState a = MyState
  { _config :: Config a
  , _actions :: [Action a]
  , _errorLog :: [ErrorLogLine]
  , _researchAreas :: ResearchAreas a
  , _resources :: Resources a
  , _seconds :: Seconds a
  , _isStarted :: IsStarted }

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
  deriving (Eq, Show)

applyAction :: Action a -> MyState a -> MyState a
applyAction (SetP p) state =
  set (resources . elements . elementPaperclips . count) p state
applyAction (SetH h) state =
  set (resources . elements . elementHelpers . count2) h state
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
  => TreeSeedCostPerTick (Cost a)
  -> [Prog a]
  -> Water a
  -> Maybe (Water a)
calcRemainingWater price ps w =
  let calculatedCost = calcWaterCost
        ps
        (unWater $ view waterCost $ unTreeSeedCostPerTick price)
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
  TwoErrors e1 e2 -> mkErrorLogLine s (paymentErrorToText e1 <> " " <> paymentErrorToText e2)

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

config :: Lens' (MyState a) (Config a)
config f state =
  (\config' -> state { _config = config' }) <$> f (_config state)

helperInc :: Lens' (Constants a) (HelperInc (Helpers a))
helperInc f state =
  (\inc' -> state { _helperInc = inc' }) <$> f (_helperInc state)

actions :: Lens' (MyState a) [Action a]
actions f state =
  (\actions' -> state { _actions = actions' }) <$> f (_actions state)

errorLog :: Lens' (MyState a) [ErrorLogLine]
errorLog f state =
  (\errorLog' -> state { _errorLog = errorLog' }) <$> f (_errorLog state)

resources :: Lens' (MyState a) (Resources a)
resources f state =
  (\resources' -> state { _resources = resources' }) <$> f (_resources state)

researchAreas :: Lens' (MyState a) (ResearchAreas a)
researchAreas f state =
  (\areas' -> state { _researchAreas = areas' }) <$> f (_researchAreas state)

seconds :: Lens' (MyState a) (Seconds a)
seconds f state =
  (\seconds' -> state { _seconds = seconds' }) <$> f (_seconds state)

isStarted :: Lens' (MyState a) IsStarted
isStarted f state =
  (\isStarted' -> state { _isStarted = isStarted' }) <$> f (_isStarted state)

researchCompProgress :: Lens' (ResearchComp a) (ResearchProgress a)
researchCompProgress f state =
  (\progress' -> state { _researchCompProgress = progress' })
    <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' (ResearchAreas a) (ResearchComp a)
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a })
  <$> f (_advancedHelperResearch state)

researchCompDuration :: Lens' (ResearchComp a) (DurationAdvancedHelper a)
researchCompDuration f state =
  (\dur -> state { _researchCompDuration = dur })
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
