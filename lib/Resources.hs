{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Resources where

import           Control.Lens                   ( Profunctor
                                                , iso
                                                , makeClassy
                                                , view
                                                )

import           Elements

newtype WaterTank a = WaterTank { unWaterTank :: a }
makeClassy ''WaterTank

data Resources a = Resources
  { _resourcesElements :: Elements a
  , _resourcesStorage :: StorageOfPaperclips a
  , _resourcesWaterTank :: WaterTank a }
makeClassy ''Resources

instance (Show a) => Show (WaterTank a) where
  show (WaterTank a) = show a

---

limitByStorage :: Ord a => StorageOfPaperclips a -> Paperclips a -> Paperclips a
limitByStorage (StorageOfPaperclips s) (Paperclips p) = Paperclips $ min s p

calcWaterCost :: Num a => [Prog a] -> a -> Water a
calcWaterCost ps waterPerSeed =
  let numberOfGrowingSeeds = fromIntegral . length . filter isGrowing $ ps
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
countNotGrowingSeeds = fromIntegral . length . filter isNotGrowing . view progs

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

---

isoWater :: (Profunctor p, Functor f) => p (Water a) (f (Water a)) -> p a (f a)
isoWater = iso Water unWater
