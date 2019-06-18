{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Resources where

import Control.Lens (Profunctor, iso, withIso)

import Elements
import NaturalTransformation
import qualified Iso

data Resources = Resources
  { _elements :: Elements
  , _storage :: Storage (Paperclips Integer)
  , _waterTank :: WaterTank Integer } deriving (Eq, Show)

newtype Storage a = Storage { unStorage :: a } deriving (Eq, Functor)

instance Show (Storage (Paperclips Integer)) where
  show (Storage a) = show a

newtype WaterTank a = WaterTank { unWaterTank :: a } deriving (Eq)

instance Show (WaterTank Integer) where
  show (WaterTank a) = show a

---

limitByStorage :: Ord a => Storage a -> a -> a
limitByStorage s = min (Iso.unwrap isoStorage s)

waterCost :: Num a => [Prog] -> a -> Water a
waterCost progs waterPerSeed = let numberOfGrowingSeeds = fromIntegral . length . filter isGrowing $ progs
      in
        Water $ waterPerSeed * numberOfGrowingSeeds

removeGrowingSeeds :: [Prog] -> [Prog]
removeGrowingSeeds = filter (not . isGrowing)

isGrowing :: Prog -> Bool
isGrowing (Growing _) = True
isGrowing _ = False

isGrowingDone :: Prog -> Bool
isGrowingDone GrowingDone = True
isGrowingDone _ = False

additionalTrees :: [Prog] -> Trees
additionalTrees = Trees . toInteger . length . filter (\x -> case x of Growing 1 -> True; _ -> False)

countNotGrowingSeeds :: Num a => TreeSeeds -> a
countNotGrowingSeeds = fromIntegral . length . filter isNotGrowing . unTreeSeeds

isNotGrowing :: Prog -> Bool
isNotGrowing a = case a of
  NotGrowing -> True
  _ -> False

progressGrowing :: [Prog] -> [Prog]
progressGrowing = map (\x -> case x of
        NotGrowing -> NotGrowing
        Growing 1 -> GrowingDone
        Growing n -> Growing (n-1)
        GrowingDone -> GrowingDone)

---

isoHelpers :: (Profunctor p, Functor f) => p (Helpers a) (f (Helpers a)) -> p a (f a)
isoHelpers = iso Helpers unHelpers

isoPaperclips :: (Profunctor p, Functor f) => p (Paperclips a) (f (Paperclips a)) -> p a (f a)
isoPaperclips = iso Paperclips unPaperclips

isoStorage :: (Profunctor p, Functor f) => p (Storage a) (f (Storage a)) -> p a (f a)
isoStorage = iso Storage unStorage

isoTreeSeeds :: (Profunctor p, Functor f) => p TreeSeeds (f TreeSeeds) -> p [Prog] (f [Prog])
isoTreeSeeds = iso TreeSeeds unTreeSeeds

isoWater :: (Profunctor p, Functor f) => p (Water a) (f (Water a)) -> p a (f a)
isoWater = iso Water unWater

helpersToPaperclips :: Helpers :~> Paperclips
helpersToPaperclips = Nat (\h -> Paperclips $ withIso isoHelpers (\_ eli -> eli h))
