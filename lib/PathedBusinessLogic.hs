module PathedBusinessLogic where

import qualified BusinessLogic as BL
import Lenses
import LensUtils
import Mod
import Resources

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

seedWork :: MyState -> Either (ErrorLogLine, [Prog]) (Water, [Prog], Trees)
seedWork = arg5 BL.seedWork
      seconds
      (resources.water)
      (config.prices.progPrice)
      (resources.treeSeeds.progs)
      (resources.trees)

buyHelper :: MyState -> Either ErrorLogLine (Helpers, Paperclips)
buyHelper = arg4 BL.buyHelper
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)

pumpWater :: MyState -> Water
pumpWater = arg2 BL.pumpWater
      (resources.water)
      (resources.waterTank)

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