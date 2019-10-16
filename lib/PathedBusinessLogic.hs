module PathedBusinessLogic where

-- The purpose of this module is to enable the type signatures
-- in BusinessLogic.hs to be very informative.

-- Note: Using lenses to rip out a minimal data set is only
-- a secondary goal.

import           Config
import           Elements
import           LensUtils
import           Resources
import           Seconds
import           Source
import           State

helperWork
      :: State a
      -> ( Paperclips a
         , Helpers a
         , HelperInc (Helpers a)
         , Storage (Paperclips a)
         )
helperWork = get4 (resources . elements . elementPaperclips . count)
                  (resources . elements . elementHelpers . count)
                  (config . constants . helperInc)
                  (resources . storage)

researchWork :: State a -> (ResearchProgress a, HelperInc (Helpers a))
researchWork = get2
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (config . constants . helperInc)

seedWork
      :: State a
      -> (Seconds a, Water a, TreeSeedCostPerTick a, [Prog a], Trees a)
seedWork = get5
      seconds
      (resources . elements . elementWater . count)
      (resources . elements . elementTrees . cost . acquireTreeSeedCostPerTick)
      (resources . elements . elementTreeSeeds . count . progs)
      (resources . elements . elementTrees . count)

buyHelper
      :: State a
      -> (Seconds a, HelpersManually a, Paperclips a, Energy a, Helpers a)
buyHelper = get5
      seconds
      (resources . elements . elementHelpers . cost . acquireHelpersManually)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementEnergy . count)
      (resources . elements . elementHelpers . count)

pumpWater :: State a -> (Water a, WaterTank a)
pumpWater =
      get2 (resources . elements . elementWater . count) (resources . waterTank)

researchAdvancedHelper
      :: State a
      -> ( Seconds a
         , Paperclips a
         , AdvancedHelperPrice (Paperclips a)
         , ResearchComp a
         )
researchAdvancedHelper = get4
      seconds
      (resources . elements . elementPaperclips . count)
      (config . prices . advancedHelperPrice)
      (researchAreas . advancedHelperResearch)

plantASeed :: State a -> (Seconds a, DurationTreeSeeds a, TreeSeeds a)
plantASeed = get3 seconds
                  (resources . elements . elementTreeSeeds . duration)
                  (resources . elements . elementTreeSeeds . count)

buyASeed :: State a -> (Seconds a, BuyTreeSeeds a, Paperclips a, TreeSeeds a)
buyASeed = get4
      seconds
      (resources . elements . elementTreeSeeds . cost . acquireBuyTreeSeeds)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementTreeSeeds . count)

generateEnergy
      :: (Enum a, Num a, Ord a, Show a)
      => State a
      -> (EnergyManually a, Energy a)
generateEnergy = get2
      (resources . elements . elementEnergy . cost . acquireEnergyManually)
      (resources . elements . elementEnergy . count)

createPaperclip :: State a -> (Paperclips a, Storage (Paperclips a))
createPaperclip = get2 (resources . elements . elementPaperclips . count)
                       (resources . storage)

extendStorage
      :: State a
      -> (Seconds a, StorageManually a, Wood a, Storage (Paperclips a))
extendStorage = get4
      seconds
      (resources . elements . elementsStorage . cost . acquireStorageManually)
      (resources . elements . elementWood . count)
      (resources . storage)

run :: State a -> (Seconds a, Paperclips a, SourceText, Storage (Paperclips a))
run = get4 seconds
           (resources . elements . elementPaperclips . count)
           (source . sourceText)
           (resources . storage)
