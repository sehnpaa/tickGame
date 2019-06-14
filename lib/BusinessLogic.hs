{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module BusinessLogic where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Text (concat, pack, Text)

import qualified Iso as Iso
import Mod
import Resources

helperWork :: (Num a, Ord a) => Paperclips a -> Helpers a -> HelperInc (Helpers a) -> Storage (Paperclips a) -> Paperclips a
helperWork p h inc storage = limitByStorage storage $ addHelperWork inc h p

limitByStorage :: Ord a => Storage a -> a -> a
limitByStorage s = min (unStorage s)

addHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Paperclips a -> Paperclips a
addHelperWork inc h p = liftA2 (+) p $ unNat helpersToPaperclips $ calcIncOfHelperWork inc h

newtype m :~> n = Nat { unNat :: forall a. m a -> n a}

helpersToPaperclips :: Helpers :~> Paperclips
helpersToPaperclips = Nat (\h -> Paperclips $ withIso Iso.helpers (\_ eli -> eli h))

calcIncOfHelperWork :: Num a => HelperInc (Helpers a) -> Helpers a -> Helpers a
calcIncOfHelperWork inc h = liftA2 (*) h $ Iso.unwrap Iso.helperInc inc

researchWork :: Num a => ResearchProgress -> HelperInc (Helpers a) -> (ResearchProgress, HelperInc (Helpers a))
researchWork progress c =
  case progress of
    NotResearched -> (progress, c)
    ResearchInProgress 1 -> (ResearchDone, twoLevels (+) c c)
    ResearchInProgress n -> (ResearchInProgress (n-1), c)
    ResearchDone -> (progress, c)

twoLevels :: (Applicative f, Applicative g) => (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
twoLevels = liftA2 . liftA2

seedWork :: Seconds -> Water -> ProgPrice -> [Prog] -> Trees -> Either (ErrorLogLine, [Prog]) (Water, [Prog], Trees)
seedWork s water price progs ts =
  let ts' = additionalTrees progs
      progs' = filter (not . isGrowingDone) $ progressGrowing progs
    in
      if any isGrowing progs
        then
          case calcRemainingWater price progs water of
            Nothing -> Left (mkErrorLogLine s "Not enough water for the seeds.", removeGrowingSeeds progs)
            Just water' -> Right $ (water', progs', ts+ts')
        else Right $ (water, progs, ts)

progressGrowing :: [Prog] -> [Prog]
progressGrowing = map (\x -> case x of
        NotGrowing -> NotGrowing
        Growing 1 -> GrowingDone
        Growing n -> Growing (n-1)
        GrowingDone -> GrowingDone)

calcRemainingWater :: ProgPrice -> [Prog] -> Water -> Maybe Water
calcRemainingWater price progs water =
  let cost = waterCost progs (unProgPrice price)
    in case cost > water of
      True -> Nothing
      False -> Just $ Iso.under2 Iso.water (-) water cost

waterCost :: [Prog] -> Integer -> Water
waterCost progs waterPerSeed = let numberOfGrowingSeeds = toInteger . length . filter isGrowing $ progs
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

buyHelper :: (Enum a, Num a, Ord a) => Seconds -> HelperPrice a -> Paperclips a -> Helpers a -> Either ErrorLogLine (Helpers a, Paperclips a)
buyHelper s price p h =
  if unHelperPrice price > p
    then Left (lineNeedMorePaperclips s)
    else Right (succ h, decPaperclipsWith price p)

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

pumpWater :: Water -> WaterTank -> Water
pumpWater w tank = Water $ min (unWaterTank tank) (succ $ unWater w)

mkErrorLogLine :: Seconds -> Text -> ErrorLogLine
mkErrorLogLine s t = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": ", t]

researchAdvancedHelper :: Seconds -> Paperclips Integer -> AdvancedHelperPrice (Paperclips Integer) -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips Integer, ResearchProgress)
researchAdvancedHelper s p price progress duration =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s "Not enough paperclips."
    (False, NotResearched) -> Right (decPaperclipsWith' price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s "Already in progress."
    (_, ResearchDone) -> Left $ mkErrorLogLine s "Already done."

decPaperclipsWith :: Num a => HelperPrice a -> Paperclips a -> Paperclips a
decPaperclipsWith = withIso (Iso.paperclips . Iso.helperPrice) (\_ eli price -> under Iso.paperclips (\p -> p - eli price))

decPaperclipsWith' :: Num a => AdvancedHelperPrice (Paperclips a) -> Paperclips a -> Paperclips a
decPaperclipsWith' hp p = withIso Iso.advancedHelperPrice (\_ eli price -> Iso.under2 Iso.paperclips (-) p (eli price)) hp

plantASeed :: Seconds -> TreeDuration -> TreePrice -> TreeSeeds -> Either ErrorLogLine TreeSeeds
plantASeed s dur price seeds =
  if unTreePrice price > countNotGrowingSeeds seeds
    then Left $ lineNeedMoreSeeds s
    else Right $ initializeSeed dur seeds

initializeSeed :: TreeDuration -> TreeSeeds -> TreeSeeds
initializeSeed duration = TreeSeeds . changeFirst (== NotGrowing) (const $ Growing $ unTreeDuration duration) . unTreeSeeds

changeFirst :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
changeFirst _ _ [] = []
changeFirst f g (x:xs) = if f x then g x : xs else x : changeFirst f g xs

countNotGrowingSeeds :: TreeSeeds -> Integer
countNotGrowingSeeds = toInteger . length . filter isNotGrowing . unTreeSeeds

isNotGrowing :: Prog -> Bool
isNotGrowing a = case a of
  NotGrowing -> True
  _ -> False

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

buyASeed :: Seconds -> TreeSeedPrice -> Paperclips Integer -> TreeSeeds -> Either ErrorLogLine (TreeSeeds, Paperclips Integer)
buyASeed s (TreeSeedPrice price) p (TreeSeeds seeds) =
  if price > p
    then Left $ mkErrorLogLine s "Not enough paperclips."
    else Right $ (TreeSeeds $ seeds ++ [NotGrowing], Iso.underAp Iso.paperclips (-) (p,price))

createPaperclip :: Paperclips Integer -> Paperclips Integer
createPaperclip = under Iso.paperclips succ
