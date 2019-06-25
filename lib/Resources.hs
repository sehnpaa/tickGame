{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Resources where

import           Control.Lens                   ( Lens'
                                                , Profunctor
                                                , iso
                                                , view
                                                , withIso
                                                )

import           Elements
import           NaturalTransformation
import qualified Iso

data Resources = Resources
  { _elements :: Elements Integer
  , _storage :: Storage (Paperclips Integer)
  , _waterTank :: WaterTank Integer }

newtype Storage a = Storage { unStorage :: a } deriving (Functor)

instance Show (Storage (Paperclips Integer)) where
  show (Storage a) = show a

newtype WaterTank a = WaterTank { unWaterTank :: a }

instance Show (WaterTank Integer) where
  show (WaterTank a) = show a

---

limitByStorage :: Ord a => Storage a -> a -> a
limitByStorage s = min (Iso.unwrap isoStorage s)

calcWaterCost :: Num a => [Prog a] -> a -> Water a
calcWaterCost progs waterPerSeed =
  let numberOfGrowingSeeds = fromIntegral . length . filter isGrowing $ progs
  in  Water $ waterPerSeed * numberOfGrowingSeeds

removeGrowingSeeds :: [Prog a] -> [Prog a]
removeGrowingSeeds = filter (not . isGrowing)

isGrowing :: Prog a -> Bool
isGrowing (Growing _) = True
isGrowing _           = False

isGrowingDone :: Prog a -> Bool
isGrowingDone GrowingDone = True
isGrowingDone _           = False

additionalTrees :: (Eq a, Num a) => [Prog a] -> Trees a
additionalTrees = Trees . fromIntegral . length . filter
  (\x -> case x of
    Growing 1 -> True
    _         -> False
  )

countNotGrowingSeeds :: Num a => TreeSeeds a -> a
countNotGrowingSeeds =
  fromIntegral . length . filter isNotGrowing . unTreeSeeds

isNotGrowing :: Prog a -> Bool
isNotGrowing a = case a of
  NotGrowing -> True
  _          -> False

progressGrowing :: (Eq a, Num a) => [Prog a] -> [Prog a]
progressGrowing = map
  (\x -> case x of
    NotGrowing  -> NotGrowing
    Growing 1   -> GrowingDone
    Growing n   -> Growing (n - 1)
    GrowingDone -> GrowingDone
  )

needMorePaperclips :: Ord a => Cost a -> Paperclips a -> Bool
needMorePaperclips c p = (view paperclipCost c) > p

needMorePaperclips' :: Ord a => BuyTreeSeeds (Cost a) -> Paperclips a -> Bool
needMorePaperclips' c p =
  unPaperclips (view paperclipCost $ unBuyTreeSeeds c) > unPaperclips p

---

elements :: Lens' Resources (Elements Integer)
elements f state =
  (\elements' -> state { _elements = elements' }) <$> f (_elements state)

storage :: Lens' Resources (Storage (Paperclips Integer))
storage f state =
    (\storage' -> state { _storage = storage' }) <$> f (_storage state)

waterTank :: Lens' Resources (WaterTank Integer)
waterTank f state =
    (\tank' -> state { _waterTank = tank' }) <$> f (_waterTank state)

isoHelpers
  :: (Profunctor p, Functor f) => p (Helpers a) (f (Helpers a)) -> p a (f a)
isoHelpers = iso Helpers unHelpers

isoPaperclips
  :: (Profunctor p, Functor f)
  => p (Paperclips a) (f (Paperclips a))
  -> p a (f a)
isoPaperclips = iso Paperclips unPaperclips

isoStorage
  :: (Profunctor p, Functor f) => p (Storage a) (f (Storage a)) -> p a (f a)
isoStorage = iso Storage unStorage

isoTreeSeeds
  :: (Profunctor p, Functor f)
  => p (TreeSeeds a) (f (TreeSeeds a))
  -> p [Prog a] (f [Prog a])
isoTreeSeeds = iso TreeSeeds unTreeSeeds

isoWater :: (Profunctor p, Functor f) => p (Water a) (f (Water a)) -> p a (f a)
isoWater = iso Water unWater

helpersToPaperclips :: Helpers :~> Paperclips
helpersToPaperclips =
  Nat (\h -> Paperclips $ withIso isoHelpers (\_ eli -> eli h))
