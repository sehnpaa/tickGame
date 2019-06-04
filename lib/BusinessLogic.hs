{-# LANGUAGE OverloadedStrings #-}

module BusinessLogic where

import Control.Lens
import Data.Text (concat, pack, Text)

import qualified Iso as Iso
import Mod
import Resources

helperWork :: Paperclips -> Helpers -> HelperInc -> Storage -> Paperclips
helperWork p h inc storage = Paperclips $ min (unStorage storage) $ (unPaperclips $ addHelperWork inc h p)

addHelperWork :: HelperInc -> Helpers -> Paperclips -> Paperclips
addHelperWork inc h p = Paperclips $ (unPaperclips p) + (unHelpers h) * (unHelpers $ unHelperInc inc)

researchWork :: ResearchProgress -> HelperInc -> (ResearchProgress, HelperInc)
researchWork progress c =
  case progress of
    NotResearched -> (progress, c)
    ResearchInProgress 1 -> (ResearchDone, incHelperIncWith c c)
    ResearchInProgress n -> (ResearchInProgress (n-1), c)
    ResearchDone -> (progress, c)

incHelperIncWith :: HelperInc -> HelperInc -> HelperInc
incHelperIncWith = withIso (Iso.helpers . Iso.helperInc) (\_ elim inc -> under (Iso.helpers . Iso.helperInc) (\h -> h + elim inc))

seedWork :: Seconds -> Water -> ProgPrice -> [Prog] -> Trees -> Either (ErrorLogLine, [Prog]) (Water, [Prog], Trees)
seedWork s water price progs ts =
  let ts' = additionalTrees progs
      progs' = filter (not . isGrowingDone) $ map (\x -> case x of
        NotGrowing -> NotGrowing
        Growing 1 -> GrowingDone
        Growing n -> Growing (n-1)
        GrowingDone -> GrowingDone) progs
    in
      if any isGrowing progs
        then
          case calcRemainingWater price progs water of
            Nothing -> Left (mkErrorLogLine s "Not enough water for the seeds.", removeGrowingSeeds progs)
            Just water' -> Right $ (water', progs', ts+ts')
        else Right $ (water, progs, ts)

calcRemainingWater :: ProgPrice -> [Prog] -> Water -> Maybe Water
calcRemainingWater price progs water =
  let cost = waterCost progs (unProgPrice price)
    in case cost > water of
      True -> Nothing
      False -> Just $ water - cost

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

buyHelper :: Seconds -> HelperPrice -> Paperclips -> Helpers -> Either ErrorLogLine (Helpers, Paperclips)
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

researchAdvancedHelper :: Seconds -> Paperclips -> AdvancedHelperPrice -> ResearchProgress -> Duration -> Either ErrorLogLine (Paperclips, ResearchProgress)
researchAdvancedHelper s p price progress duration =
  case (unAdvancedHelperPrice price > p, progress) of
    (True, NotResearched) -> Left $ mkErrorLogLine s "Not enough paperclips."
    (False, NotResearched) -> Right (decPaperclipsWith' price p, startResearch duration)
    (_, ResearchInProgress _) -> Left $ mkErrorLogLine s "Already in progress."
    (_, ResearchDone) -> Left $ mkErrorLogLine s "Already done."

decPaperclipsWith :: HelperPrice -> Paperclips -> Paperclips
decPaperclipsWith = withIso (Iso.paperclips . Iso.helperPrice) (\_ elim price -> under Iso.paperclips (\p -> p - elim price))

decPaperclipsWith' :: AdvancedHelperPrice -> Paperclips -> Paperclips
decPaperclipsWith' = withIso (Iso.paperclips . Iso.advancedHelperPrice) (\_ elim price -> under Iso.paperclips (\p -> p - elim price))

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

buyASeed :: Seconds -> TreeSeedPrice -> Paperclips -> TreeSeeds -> Either ErrorLogLine (TreeSeeds, Paperclips)
buyASeed s (TreeSeedPrice price) p (TreeSeeds seeds) =
  if price > p
    then Left $ mkErrorLogLine s "Not enough paperclips."
    else Right $ (TreeSeeds $ seeds ++ [NotGrowing], Iso.underPaperclips (-) p price)

createPaperclip :: Paperclips -> Paperclips
createPaperclip = under Iso.paperclips succ
