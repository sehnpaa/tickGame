module PathedBusinessLogic where

import qualified BusinessLogic as BL
import Lenses
import LensUtils
import Mod

helperWork :: MyState -> Paperclips
helperWork = arg4 BL.helperWork
      (resources.paperclips)
      (resources.helpers)
      (config.constants.helperInc)
      (resources.storage)

researchWork :: MyState -> (ResearchProgress, HelperInc)
researchWork = arg2 BL.researchWork
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (config.constants.helperInc)

seedWork :: MyState -> ([Prog], Trees)
seedWork = arg2 BL.seedWork
      (resources.treeSeeds.progs)
      (resources.trees)

buyHelper :: MyState -> Either ErrorLogLine (Helpers, Paperclips)
buyHelper = arg4 BL.buyHelper
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)

pumpWater :: MyState -> Water
pumpWater = arg1 BL.pumpWater
      (resources.water)

researchAdvancedHelper :: MyState -> Either ErrorLogLine (Paperclips, ResearchProgress)
researchAdvancedHelper = arg5 BL.researchAdvancedHelper
      seconds
      (resources.paperclips)
      (config.prices.advancedHelperPrice)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (researchAreas.advancedHelperResearch.researchCompDuration)

plantASeed :: MyState -> Either ErrorLogLine TreeSeeds
plantASeed = arg4 BL.plantASeed
      seconds
      (config.durations.treeDuration)
      (config.prices.treePrice)
      (resources.treeSeeds)

createPaperclip :: MyState -> Paperclips
createPaperclip = arg1 BL.createPaperclip
      (resources.paperclips)