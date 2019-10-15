{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Elements where

import           Control.Lens
import           Control.Applicative            ( liftA2 )
import qualified Data.Text                     as T

data Duration a = Instant | Ticks a

newtype DurationPaperclips a = DurationPaperclips { unDurationPaperclips :: Duration a }
newtype DurationEnergy a = DurationEnergy { unDurationEnergy :: Duration a }
newtype DurationHelpers a = DurationHelpers { unDurationHelpers :: Duration a }
newtype DurationTrees a = DurationTrees { unDurationTrees :: Duration a }
newtype DurationTreeSeeds a = DurationTreeSeeds { _durationTreeSeeds :: Duration a }
makeLenses ''DurationTreeSeeds

newtype DurationWater a = DurationWater { unDurationWater :: Duration a }
newtype DurationWood a = DurationWood { unDurationWood :: Duration a }

data Paperclips a = Paperclips { _paperclips :: a } deriving (Eq, Functor, Ord)
makeLenses ''Paperclips

instance Applicative Paperclips where
  pure = Paperclips
  Paperclips f <*> Paperclips a = Paperclips (f a)

instance Show (Paperclips Integer) where
  show (Paperclips a) = show a

newtype Energy a = Energy { _energy :: a } deriving (Eq, Functor, Ord)
makeLenses ''Energy

instance Applicative Energy where
  pure = Energy
  Energy f <*> Energy a = Energy (f a)

instance Show (Energy Integer) where
  show (Energy a) = show a

newtype Helpers a = Helpers { unHelpers :: a } deriving (Enum, Eq, Functor, Ord)

instance Applicative Helpers where
  pure = Helpers
  Helpers f <*> Helpers a = Helpers (f a)

instance Show (Helpers Integer) where
  show (Helpers a) = show a

newtype Trees a = Trees { unTrees :: a } deriving (Enum, Eq, Num, Ord)

instance Show (Trees Integer) where
  show (Trees a) = show a

data Prog a = NotGrowing | Growing a | GrowingDone deriving (Eq)

instance Show (Prog Integer) where
  show NotGrowing = show "Not growing"
  show (Growing n) =
    show "Growing in progress - " ++ show n ++ " " ++ noun n ++ " left."
   where
    noun 1 = "tick"
    noun _ = "ticks"
  show GrowingDone = show "Growing done"

newtype TreeSeeds a = TreeSeeds { _treeSeeds :: [Prog a]} deriving (Eq)
makeLenses ''TreeSeeds

instance Show (TreeSeeds Integer) where
  show (TreeSeeds a) = show a

newtype Water a = Water { unWater :: a } deriving (Eq, Ord)

instance Show (Water Integer) where
  show (Water a) = show a

newtype Wood a = Wood { unWood :: a } deriving (Enum, Eq, Ord)

instance Show (Wood Integer) where
  show (Wood a) = show a

data Cost a = Cost
  { _paperclipCost :: Paperclips a
  , _energyCost :: Energy a
  , _helpersCost :: Helpers a
  , _treesCost :: Trees a
  , _treeSeedsCost :: TreeSeeds a
  , _waterCost :: Water a
  , _woodCost :: Wood a }
makeLenses ''Cost

data NoCost a = NoCost

data CostEnergyPaperclips a = CostEnergyPaperclips (Energy a) (Paperclips a)
data CostPaperclips a = CostPaperclips { _costPaperclips :: Paperclips a }
makeLenses ''CostPaperclips
data CostWater a = CostWater { _costWater :: Water a }
makeLenses ''CostWater

data CostTreeSeeds a = CostTreeSeeds (TreeSeeds [Prog a])

newtype PaperclipsManually a = PaperclipsManually { unPaperclipsManually :: NoCost a }
newtype PaperclipsFromHelper a = PaperclipsFromHelper { unPaperclipsFromHelper :: NoCost a }

data AcquirePaperclips a = AcquirePaperclips
  { _paperclipsManually :: PaperclipsManually a
  , _paperclipsFromHelpers :: PaperclipsFromHelper a }
makeLenses ''AcquirePaperclips

data BuyTreeSeeds a = BuyTreeSeeds { unBuyTreeSeeds :: CostPaperclips a, buyTreeSeedsErrorMessage :: T.Text }

newtype AcquireTreeSeeds a = AcquireTreeSeeds { _acquireBuyTreeSeeds :: BuyTreeSeeds a }
makeLenses ''AcquireTreeSeeds

newtype TreesFromTreeSeeds a = TreesFromTreeSeeds { unTreesFromTreeSeeds :: CostTreeSeeds a }
newtype TreeSeedCostPerTick a = TreeSeedCostPerTick { unTreeSeedCostPerTick :: CostWater a }

data EnergyManually a = EnergyManually { _energyManually :: NoCost a}

data AcquireEnergy cost = AcquireEnergy
  { _acquireEnergyManually :: EnergyManually cost }
makeLenses ''AcquireEnergy

newtype HelpersManually a = HelpersManually { _helpersManually :: CostEnergyPaperclips a}
makeLenses ''HelpersManually

data AcquireHelpers a = AcquireHelpers
  { _acquireHelpersManually :: HelpersManually a }
makeLenses ''AcquireHelpers

data AcquireTrees a = AcquireTrees
  { _acquireTreesFromTreeSeeds :: TreesFromTreeSeeds a
  , _acquireTreeSeedCostPerTick :: TreeSeedCostPerTick a }
makeLenses ''AcquireTrees

newtype WaterManually a = WaterManually { unWaterManually :: NoCost a }
newtype AcquireWater a = AcquireWater { _acquireWater :: WaterManually a }

newtype WoodManually a = WoodManually { unWoodManually :: NoCost a }
newtype AcquireWood a = AcquireWood { _acquireWood :: WoodManually a }

data Element acquire duration f a = Element
  { _cost :: acquire a
  , _count :: f a
  , _duration :: duration a }
makeLenses ''Element

data Elements a = Elements
  { _elementsPaperclips :: Element AcquirePaperclips DurationPaperclips Paperclips a
  , _elementEnergy :: Element AcquireEnergy DurationEnergy Energy a
  , _helpers :: Element AcquireHelpers DurationHelpers Helpers a
  , _elementTrees :: Element AcquireTrees DurationTrees Trees a
  , _elementsTreeSeeds :: Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a
  , _water :: Element AcquireWater DurationWater Water a
  , _wood :: Element AcquireWood DurationWood Wood a }
makeLenses ''Elements

elementPaperclips
  :: Lens'
       (Elements a)
       (Element AcquirePaperclips DurationPaperclips Paperclips a)
elementPaperclips f state =
  (\paperclips' -> state { _elementsPaperclips = paperclips' })
    <$> f (_elementsPaperclips state)

elementHelpers
  :: Lens' (Elements a) (Element AcquireHelpers DurationHelpers Helpers a)
elementHelpers f state =
  (\helpers' -> state { _helpers = helpers' }) <$> f (_helpers state)

elementWater :: Lens' (Elements a) (Element AcquireWater DurationWater Water a)
elementWater f state =
  (\water' -> state { _water = water' }) <$> f (_water state)

elementTreeSeeds
  :: Lens' (Elements a) (Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a)
elementTreeSeeds f state =
  (\treeSeeds' -> state { _elementsTreeSeeds = treeSeeds' })
    <$> f (_elementsTreeSeeds state)

progs :: Lens' (TreeSeeds a) [Prog a]
progs f state = TreeSeeds <$> f (view treeSeeds state)

elementWood :: Lens' (Elements a) (Element AcquireWood DurationWood Wood a)
elementWood f state = (\wood' -> state { _wood = wood' }) <$> f (_wood state)

data PaymentError = PaymentError | NotEnoughEnergy | NotEnoughPaperclips

paymentErrorToText :: PaymentError -> T.Text
paymentErrorToText e = case e of
  PaymentError        -> T.pack "Paymenterror"
  NotEnoughEnergy     -> T.pack "Not enough energy."
  NotEnoughPaperclips -> T.pack "Not enough paperclips."

data ErrorCount a = OneError a | TwoErrors a a

payWith
  :: (Ord (f c), Num c, Applicative f)
  => Getting (f c) s (f c)
  -> f c
  -> s
  -> Either PaymentError (f c)
payWith f p c =
  let c' = view f c
  in  if c' <= p then Right $ liftA2 (-) p c' else Left PaymentError

payWithPaperclips
  :: (Num a, Ord a)
  => Paperclips a
  -> CostPaperclips a
  -> Either PaymentError (Paperclips a)
payWithPaperclips = payWith costPaperclips

payWithEnergy
  :: (Num a, Ord a) => Energy a -> Cost a -> Either PaymentError (Energy a)
payWithEnergy = payWith energyCost

freeEnergy :: (Enum a) => NoCost a -> Energy a -> Energy a
freeEnergy = const (fmap succ)

-- toWallet paperclipCost
-- toWallet :: Monoid m => ASetter (Cost m) t a b -> b -> t
-- toWallet f p = set f p noCostG

-- noCostG :: (Monoid a) => Cost a
-- noCostG = Cost (Paperclips mempty)
--                (Helpers mempty)
--                (Trees mempty)
--                (TreeSeeds mempty)
--                (Water mempty)
--                (Wood mempty)

-- pPaperclips
--   :: (Num a, Ord a) => (Cost a) -> Paperclips a -> Either PaymentError (Paperclips a)
-- pPaperclips need owning =
--   let n = view (paperclipCost . paperclips) need
--       o = view paperclips owning
--   in  if o >= n then Right $ Paperclips $ o - n else Left PaymentError

calcEnergyPaperclipsCombo
  :: (Num a, Ord a)
  => CostEnergyPaperclips a
  -> Energy a
  -> Paperclips a
  -> Either (ErrorCount PaymentError) (Energy a, Paperclips a)
calcEnergyPaperclipsCombo (CostEnergyPaperclips ce cp) e p =
  case (enoughEnergy ce e, enoughPaperclips cp p) of
    (Just e1, Just e2) -> Left $ TwoErrors e1 e2
    (Just e1, Nothing) -> Left $ OneError e1
    (Nothing, Just e2) -> Left $ OneError e2
    (Nothing, Nothing) -> Right (liftA2 (-) e ce, liftA2 (-) p cp)

enoughEnergy :: (Ord a) => Energy a -> Energy a -> Maybe PaymentError
enoughEnergy (Energy ce) (Energy e) =
  if ce <= e then Nothing else Just NotEnoughEnergy

enoughPaperclips
  :: (Ord a) => Paperclips a -> Paperclips a -> Maybe PaymentError
enoughPaperclips (Paperclips cp) (Paperclips p) =
  if cp <= p then Nothing else Just NotEnoughPaperclips

calcPaperclips
  :: (Num a, Ord a) => CostPaperclips a -> Paperclips a -> Maybe (Paperclips a)
calcPaperclips (CostPaperclips c) p =
  if c <= p then Just (liftA2 (-) p c) else Nothing
