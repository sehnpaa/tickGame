module Lenses where

import           Control.Lens

import           Config
import           Elements
import           Mod
import           Resources

config :: Lens' MyState Config
config f state =
    (\config' -> state { _config = config' }) <$> f (_config state)

constants :: Lens' Config Constants
constants f state =
    (\constants' -> state { _constants = constants' }) <$> f (_constants state)

treeSeedDuration :: Lens' (Element acquirement d f a) (d (Duration a))
treeSeedDuration f state =
    (\a' -> state { _duration2 = a' }) <$> f (_duration2 state)

helperInc :: Lens' Constants (HelperInc (Helpers Integer))
helperInc f state =
    (\inc' -> state { _helperInc = inc' }) <$> f (_helperInc state)

prices :: Lens' Config (Prices Integer)
prices f state =
    (\prices' -> state { _prices = prices' }) <$> f (_prices state)

advancedHelperPrice
    :: Lens' (Prices Integer) (AdvancedHelperPrice (Paperclips Integer))
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

elements :: Lens' Resources Elements
elements f state =
    (\elements' -> state { _elements = elements' }) <$> f (_elements state)

cost :: Lens' (Element acquirement d f a) (acquirement (Cost a))
cost f state = (\a -> state { _cost = a }) <$> f (_cost state)

helpersManually
    :: Lens' (AcquireHelpers (Cost Integer)) (HelpersManually (Cost Integer))
helpersManually f state =
    (\a -> state { _helpersManually = a }) <$> f (_helpersManually state)

buyTreeSeeds
    :: Lens' (AcquireTreeSeeds (Cost Integer)) (BuyTreeSeeds (Cost Integer))
buyTreeSeeds f state = AcquireTreeSeeds <$> f (unAcquireTreeSeeds state)

count :: Lens' (Element ac d f a) (f a)
count f state = (\a -> state { _count = a }) <$> f (_count state)

elementPaperclips
    :: Lens'
           Elements
           (Element AcquirePaperclips DurationPaperclips Paperclips Integer)
elementPaperclips f state =
    (\paperclips' -> state { _paperclips = paperclips' })
        <$> f (_paperclips state)

paperclips :: Lens' Elements (Paperclips Integer)
paperclips = elementPaperclips . count

elementHelpers
    :: Lens' Elements (Element AcquireHelpers DurationHelpers Helpers Integer)
elementHelpers f state =
    (\helpers' -> state { _helpers = helpers' }) <$> f (_helpers state)

helpers :: Lens' Elements (Helpers Integer)
helpers = elementHelpers . count

storage :: Lens' Resources (Storage (Paperclips Integer))
storage f state =
    (\storage' -> state { _storage = storage' }) <$> f (_storage state)

elementWater
    :: Lens' Elements (Element AcquireWater DurationWater Water Integer)
elementWater f state =
    (\water' -> state { _water = water' }) <$> f (_water state)

water :: Lens' Elements (Water Integer)
water = elementWater . count

waterTank :: Lens' Resources (WaterTank Integer)
waterTank f state =
    (\tank' -> state { _waterTank = tank' }) <$> f (_waterTank state)

elementTrees
    :: Lens' Elements (Element AcquireTrees DurationTrees Trees Integer)
elementTrees f state =
    (\trees' -> state { _trees = trees' }) <$> f (_trees state)

trees :: Lens' Elements (Trees Integer)
trees = elementTrees . count

elementTreeSeeds
    :: Lens'
           Elements
           (Element AcquireTreeSeeds DurationTreeSeeds TreeSeeds Integer)
elementTreeSeeds f state =
    (\treeSeeds' -> state { _treeSeeds = treeSeeds' }) <$> f (_treeSeeds state)

treeSeeds :: Lens' Elements (TreeSeeds Integer)
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
    :: Lens' (ResearchComp Integer) (DurationAdvancedHelper (Duration Integer))
researchCompDuration f state =
    undefined (\duration -> state { _researchCompDuration = duration })
        <$> f (_researchCompDuration state)

seconds :: Lens' MyState Seconds
seconds f state =
    (\seconds' -> state { _seconds = seconds' }) <$> f (_seconds state)

elementWood :: Lens' Elements (Element AcquireWood DurationWood Wood Integer)
elementWood f state = (\wood' -> state { _wood = wood' }) <$> f (_wood state)

wood :: Lens' Elements (Wood Integer)
wood = elementWood . count

isStarted :: Lens' MyState IsStarted
isStarted f state =
    (\isStarted' -> state { _isStarted = isStarted' }) <$> f (_isStarted state)
