module PathedBusinessLogic where

import qualified BusinessLogic as BL
import LensUtils
import Mod

helperWork :: MyState -> Paperclips
helperWork = arg4 BL.helperWork
      (resources.paperclips)
      (resources.helpers)
      (config.constants.helperInc)
      (resources.storage)

buyHelper :: MyState -> Either ErrorLogLine (Helpers, Paperclips)
buyHelper = arg4 BL.buyHelper
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)

researchAdvancedHelper :: MyState -> Either ErrorLogLine (Paperclips, ResearchProgress)
researchAdvancedHelper = arg5 BL.researchAdvancedHelper
      seconds
      (resources.paperclips)
      (config.prices.advancedHelperPrice)
      (researchAreas.advancedHelperResearch.researchCompProgress)
      (researchAreas.advancedHelperResearch.researchCompDuration)

plantASeed :: MyState -> Either ErrorLogLine (TreeSeeds, Trees)
plantASeed = arg4 BL.plantASeed
      seconds
      (config.prices.treePrice)
      (resources.treeSeeds)
      (resources.trees)