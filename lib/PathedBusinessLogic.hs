{-# LANGUAGE RankNTypes #-}

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
         , StorageOfPaperclips a
         )
helperWork = get4
      (stateResources . resourcesElements . elementsPaperclips . count)
      (stateResources . resourcesElements . elementsHelpers . count)
      (stateConfig . configConstants . helperInc)
      (stateResources . resourcesStorage)

researchWork :: State a -> (ResearchProgress a, HelperInc (Helpers a))
researchWork = get2
      (stateResearchAreas . advancedHelperResearch . researchCompProgress)
      (stateConfig . configConstants . helperInc)

seedWork
      :: State a
      -> (Seconds a, Water a, TreeSeedCostPerTick a, [Prog a], Trees a)
seedWork = get5
      stateSeconds
      (stateResources . resourcesElements . elementsWater . count)
      ( stateResources
      . resourcesElements
      . elementsTrees
      . elementCost
      . acquireTreeSeedCostPerTick
      )
      (stateResources . resourcesElements . elementsTreeSeeds . count . progs)
      (stateResources . resourcesElements . elementsTrees . count)

buyHelper
      :: State a
      -> (Seconds a, HelpersManually a, Paperclips a, Energy a, Helpers a)
buyHelper = get5
      stateSeconds
      ( stateResources
      . resourcesElements
      . elementsHelpers
      . elementCost
      . acquireHelpersManually
      )
      (stateResources . resourcesElements . elementsPaperclips . count)
      (stateResources . resourcesElements . elementsEnergy . count)
      (stateResources . resourcesElements . elementsHelpers . count)

pumpWater :: State a -> (Water a, WaterTank a)
pumpWater = get2
      (stateResources . resourcesElements . elementsWater . count)
      (stateResources . resourcesWaterTank)

researchAdvancedHelper
      :: State a
      -> ( Seconds a
         , Paperclips a
         , AdvancedHelperPrice (Paperclips a)
         , ResearchComp a
         )
researchAdvancedHelper = get4
      stateSeconds
      (stateResources . resourcesElements . elementsPaperclips . count)
      (stateConfig . configPrices . advancedHelperPrice)
      (stateResearchAreas . advancedHelperResearch)

plantASeed :: State a -> (Seconds a, DurationTreeSeeds a, TreeSeeds a)
plantASeed = get3
      stateSeconds
      (stateResources . resourcesElements . elementsTreeSeeds . duration)
      (stateResources . resourcesElements . elementsTreeSeeds . count)

buyASeed :: State a -> (Seconds a, BuyTreeSeeds a, Paperclips a, TreeSeeds a)
buyASeed = get4
      stateSeconds
      ( stateResources
      . resourcesElements
      . elementsTreeSeeds
      . elementCost
      . acquireBuyTreeSeeds
      )
      (stateResources . resourcesElements . elementsPaperclips . count)
      (stateResources . resourcesElements . elementsTreeSeeds . count)

extendStorage
      :: State a
      -> (Seconds a, StorageManually a, Wood a, StorageOfPaperclips a)
extendStorage = get4
      stateSeconds
      ( stateResources
      . resourcesElements
      . elementsStorage
      . elementCost
      . acquireStorageManually
      )
      (stateResources . resourcesElements . elementsWood . count)
      (stateResources . resourcesStorage)

run :: State a -> (Seconds a, Paperclips a, SourceText, StorageOfPaperclips a)
run = get4
      stateSeconds
      (stateResources . resourcesElements . elementsPaperclips . count)
      (stateSource . sourceText)
      (stateResources . resourcesStorage)
