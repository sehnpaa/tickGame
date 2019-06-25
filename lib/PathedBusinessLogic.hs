module PathedBusinessLogic where

import qualified BusinessLogic                 as BL
import           Config
import           Elements
import           LensUtils
import           Mod
import           Resources

helperWork :: MyState -> Paperclips Integer
helperWork = arg4 BL.helperWork
                  (resources . elements . paperclips)
                  (resources . elements . helpers)
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
                (resources . elements . water)
                (config . prices . progPrice)
                (resources . elements . treeSeeds . progs)
                (resources . elements . trees)

buyHelper
      :: MyState -> Either ErrorLogLine (Helpers Integer, Paperclips Integer)
buyHelper = arg4
      BL.buyHelper
      seconds
      (resources . elements . elementHelpers . cost . helpersManually)
      (resources . elements . paperclips)
      (resources . elements . helpers)

pumpWater :: MyState -> Water Integer
pumpWater =
      arg2 BL.pumpWater (resources . elements . water) (resources . waterTank)

researchAdvancedHelper
      :: MyState -> Either ErrorLogLine (Paperclips Integer, ResearchProgress)
researchAdvancedHelper = arg5
      BL.researchAdvancedHelper
      seconds
      (resources . elements . paperclips)
      (config . prices . advancedHelperPrice)
      (researchAreas . advancedHelperResearch . researchCompProgress)
      (researchAreas . advancedHelperResearch . researchCompDuration)

plantASeed :: MyState -> Either ErrorLogLine (TreeSeeds Integer)
plantASeed = arg4 BL.plantASeed
                  seconds
                  (resources . elements . elementTreeSeeds . treeSeedDuration)
                  (config . prices . treePrice)
                  (resources . elements . treeSeeds)

buyASeed
      :: MyState -> Either ErrorLogLine (TreeSeeds Integer, Paperclips Integer)
buyASeed = arg4
      BL.buyASeed
      seconds
      (resources . elements . elementTreeSeeds . cost . buyTreeSeeds)
      (resources . elements . paperclips)
      (resources . elements . treeSeeds)

createPaperclip :: MyState -> Paperclips Integer
createPaperclip = arg2 BL.createPaperclip
                       (resources . elements . paperclips)
                       (resources . storage)
