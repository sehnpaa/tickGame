module PathedBusinessLogic where

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import           LensUtils
import           Mod
import           Resources

helperWork :: MyState -> Paperclips Integer
helperWork = arg4 BL.helperWork
                  (resources . elements . elementPaperclips . count)
                  (resources . elements . elementHelpers . count)
                  (config . constants . helperInc)
                  (resources . storage)

researchWork :: MyState -> (ResearchProgress, HelperInc (Helpers Integer))
researchWork = arg2
      BL.researchWork
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (config . constants . helperInc)

seedWork
      :: MyState
      -> Either
               (ErrorLogLine, [Prog Integer])
               (Water Integer, [Prog Integer], Trees Integer)
seedWork = arg5 BL.seedWork
                seconds
                (resources . elements . elementWater . count)
                (config . prices . progPrice)
                (resources . elements . elementTreeSeeds . count . progs)
                (resources . elements . elementTrees . count)

buyHelper
      :: MyState -> Either ErrorLogLine (Helpers Integer, Paperclips Integer)
buyHelper = arg4
      BL.buyHelper
      seconds
      (resources . elements . elementHelpers . cost . helpersManually)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementHelpers . count)

pumpWater :: MyState -> Water Integer
pumpWater = arg2 BL.pumpWater
                 (resources . elements . elementWater . count)
                 (resources . waterTank)

researchAdvancedHelper
      :: MyState -> Either ErrorLogLine (Paperclips Integer, ResearchProgress)
researchAdvancedHelper = arg5
      BL.researchAdvancedHelper
      seconds
      (resources . elements . elementPaperclips . count)
      (config . prices . advancedHelperPrice)
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (researchAreas . advancedHelperResearch . researchCompDuration)

plantASeed :: MyState -> Either ErrorLogLine (TreeSeeds Integer)
plantASeed = arg4 BL.plantASeed
                  seconds
                  (resources . elements . elementTreeSeeds . duration)
                  (config . prices . treePrice)
                  (resources . elements . elementTreeSeeds . count)

buyASeed
      :: MyState -> Either ErrorLogLine (TreeSeeds Integer, Paperclips Integer)
buyASeed = arg4
      BL.buyASeed
      seconds
      (resources . elements . elementTreeSeeds . cost . buyTreeSeeds)
      (resources . elements . elementPaperclips . count)
      (resources . elements . elementTreeSeeds . count)

createPaperclip :: MyState -> Paperclips Integer
createPaperclip = arg2 BL.createPaperclip
                       (resources . elements . elementPaperclips . count)
                       (resources . storage)
