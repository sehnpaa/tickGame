module Lenses where

import           Control.Lens

import           Config
import           Elements
import           Mod
import           Resources

config :: Lens' MyState Config
config f state =
    (\config' -> state { _config = config' }) <$> f (_config state)

constants :: Lens' Config (Constants Integer)
constants f state =
    (\constants' -> state { _constants = constants' }) <$> f (_constants state)

treeSeedDuration :: Lens' (Element acquirement d f a) (d a)
treeSeedDuration f state =
    (\a' -> state { _duration = a' }) <$> f (_duration state)

helperInc :: Lens' (Constants a) (HelperInc (Helpers a))
helperInc f state =
    (\inc' -> state { _helperInc = inc' }) <$> f (_helperInc state)

prices :: Lens' Config (Prices Integer)
prices f state =
    (\prices' -> state { _prices = prices' }) <$> f (_prices state)

advancedHelperPrice
    :: Lens' (Prices a) (AdvancedHelperPrice (Paperclips a))
advancedHelperPrice f state =
    (\price' -> state { _advancedHelperPrice = price' })
        <$> f (_advancedHelperPrice state)

treePrice :: Lens' (Prices a) (TreePrice a)
treePrice f state =
    (\treePrice' -> state { _treePrice = treePrice' }) <$> f (_treePrice state)

progPrice :: Lens' (Prices a) (ProgPrice a)
progPrice f state =
    (\price' -> state { _progPrice = price' }) <$> f (_progPrice state)

actions :: Lens' MyState [Action]
actions f state =
    (\actions' -> state { _actions = actions' }) <$> f (_actions state)

errorLog :: Lens' MyState [ErrorLogLine]
errorLog f state =
    (\errorLog' -> state { _errorLog = errorLog' }) <$> f (_errorLog state)

resources :: Lens' MyState Resources
resources f state =
    (\resources' -> state { _resources = resources' }) <$> f (_resources state)

elements :: Lens' Resources (Elements Integer)
elements f state =
    (\elements' -> state { _elements = elements' }) <$> f (_elements state)

cost :: Lens' (Element acquirement d f a) (acquirement (Cost a))
cost f state = (\a -> state { _cost = a }) <$> f (_cost state)

helpersManually
    :: Lens' (AcquireHelpers (Cost a)) (HelpersManually (Cost a))
helpersManually f state =
    (\a -> state { _helpersManually = a }) <$> f (_helpersManually state)

buyTreeSeeds
    :: Lens' (AcquireTreeSeeds (Cost a)) (BuyTreeSeeds (Cost a))
buyTreeSeeds f state = AcquireTreeSeeds <$> f (unAcquireTreeSeeds state)

count :: Lens' (Element ac d f a) (f a)
count f state = (\a -> state { _count = a }) <$> f (_count state)

elementPaperclips
    :: Lens'
           (Elements a)
           (Element AcquirePaperclips DurationPaperclips Paperclips a)
elementPaperclips f state =
    (\paperclips' -> state { _paperclips = paperclips' })
        <$> f (_paperclips state)

paperclips :: Lens' (Elements a) (Paperclips a)
paperclips = elementPaperclips . count

elementHelpers
    :: Lens' (Elements a) (Element AcquireHelpers DurationHelpers Helpers a)
elementHelpers f state =
    (\helpers' -> state { _helpers = helpers' }) <$> f (_helpers state)

helpers :: Lens' (Elements a) (Helpers a)
helpers = elementHelpers . count

storage :: Lens' Resources (Storage (Paperclips Integer))
storage f state =
    (\storage' -> state { _storage = storage' }) <$> f (_storage state)

elementWater
    :: Lens' (Elements a) (Element AcquireWater DurationWater Water a)
elementWater f state =
    (\water' -> state { _water = water' }) <$> f (_water state)

water :: Lens' (Elements a) (Water a)
water = elementWater . count

waterTank :: Lens' Resources (WaterTank Integer)
waterTank f state =
    (\tank' -> state { _waterTank = tank' }) <$> f (_waterTank state)

elementTrees
    :: Lens' (Elements a) (Element AcquireTrees DurationTrees Trees a)
elementTrees f state =
    (\trees' -> state { _trees = trees' }) <$> f (_trees state)

trees :: Lens' (Elements a) (Trees a)
trees = elementTrees . count

elementTreeSeeds
    :: Lens'
           (Elements a)
           (Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds a)
elementTreeSeeds f state =
    (\treeSeeds' -> state { _treeSeeds = treeSeeds' }) <$> f (_treeSeeds state)

treeSeeds :: Lens' (Elements a) (TreeSeeds a)
treeSeeds = elementTreeSeeds . count

progs :: Lens' (TreeSeeds Integer) [Prog Integer]
progs f state = TreeSeeds <$> f (unTreeSeeds state)

researchAreas :: Lens' MyState ResearchAreas
researchAreas f state =
    (\areas' -> state { _researchAreas = areas' }) <$> f (_researchAreas state)

researchCompProgress :: Lens' (ResearchComp Integer) ResearchProgress
researchCompProgress f state =
    (\progress' -> state { _researchCompProgress = progress' })
        <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' ResearchAreas (ResearchComp Integer)
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a })
    <$> f (_advancedHelperResearch state)

researchCompDuration
    :: Lens' (ResearchComp a) (DurationAdvancedHelper a)
researchCompDuration f state =
    undefined (\duration -> state { _researchCompDuration = duration })
        <$> f (_researchCompDuration state)

seconds :: Lens' MyState Seconds
seconds f state =
    (\seconds' -> state { _seconds = seconds' }) <$> f (_seconds state)

elementWood :: Lens' (Elements a) (Element AcquireWood DurationWood Wood a)
elementWood f state = (\wood' -> state { _wood = wood' }) <$> f (_wood state)

wood :: Lens' (Elements a) (Wood a)
wood = elementWood . count

isStarted :: Lens' MyState IsStarted
isStarted f state =
    (\isStarted' -> state { _isStarted = isStarted' }) <$> f (_isStarted state)
