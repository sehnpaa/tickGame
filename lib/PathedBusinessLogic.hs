module PathedBusinessLogic where

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import           LensUtils
import           Mod
import           Resources

helperWork :: (Num a, Ord a) => MyState a -> Paperclips a
helperWork = arg4 BL.helperWork
                  (resources . elements . elementPaperclips . count)
                  (resources . elements . elementHelpers . count)
                  (config . constants . helperInc)
                  (resources . storage)

researchWork
      :: (Eq a, Num a)
      => MyState a
      -> (ResearchProgress a, HelperInc (Helpers a))
researchWork = arg2
      BL.researchWork
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (config . constants . helperInc)

seedWork
      :: (Num a, Ord a, Show a)
      => MyState a
      -> Either (ErrorLogLine, [Prog a]) (Water a, [Prog a], Trees a)
seedWork = arg5
      BL.seedWork
      seconds
      (resources . elements . elementWater . count)
      (resources . elements . elementTrees . cost . treeSeedCostPerTick)
      (resources . elements . elementTreeSeeds . count . progs)
      (resources . elements . elementTrees . count)

buyHelper
      :: (Enum a, Num a, Ord a, Show a)
      => MyState a
      -> Either ErrorLogLine (Helpers a, Paperclips a)
buyHelper = arg4
      BL.buyHelper
      seconds
      (resources . elements . elementHelpers . cost . helpersManually)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementHelpers . count)

pumpWater :: (Enum a, Num a, Ord a) => MyState a -> Water a
pumpWater = arg2 BL.pumpWater
                 (resources . elements . elementWater . count)
                 (resources . waterTank)

researchAdvancedHelper
      :: (Num a, Ord a, Show a)
      => MyState a
      -> Either ErrorLogLine (Paperclips a, ResearchProgress a)
researchAdvancedHelper = arg5
      BL.researchAdvancedHelper
      seconds
      (resources . elements . elementPaperclips . count)
      (config . prices . advancedHelperPrice)
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (researchAreas . advancedHelperResearch . researchCompDuration)

plantASeed
      :: (Num a, Ord a, Show a)
      => MyState a
      -> Either ErrorLogLine (TreeSeeds a)
plantASeed = arg3 BL.plantASeed
                  seconds
                  (resources . elements . elementTreeSeeds . duration)
                  (resources . elements . elementTreeSeeds . count)

buyASeed
      :: (Num a, Ord a, Show a)
      => MyState a
      -> Either ErrorLogLine (TreeSeeds a, Paperclips a)
buyASeed = arg4
      BL.buyASeed
      seconds
      (resources . elements . elementTreeSeeds . cost . buyTreeSeeds)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementTreeSeeds . count)

generateEnergy
      :: (Enum a, Num a, Ord a, Show a)
      => MyState a
      -> Either ErrorLogLine (Energy a)
generateEnergy = arg3
      BL.generateEnergy
      seconds
      (resources . elements . elementEnergy . cost . acquireEnergyManually)
      (resources . elements . elementEnergy . count)

createPaperclip :: (Enum a, Ord a) => MyState a -> Paperclips a
createPaperclip = arg2 BL.createPaperclip
                       (resources . elements . elementPaperclips . count)
                       (resources . storage)
