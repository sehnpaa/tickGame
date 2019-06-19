module Lenses where

import Control.Lens

import Config
import Elements
import Mod
import Resources

config :: Lens' MyState Config
config f state = (\config' -> state { _config = config'}) <$> f (_config state)

constants :: Lens' Config Constants
constants f state = (\constants' -> state { _constants = constants'}) <$> f (_constants state)

durations :: Lens' Config Durations
durations f state = (\durations' -> state { _durations = durations'}) <$> f (_durations state)

treeDuration :: Lens' Durations TreeDuration
treeDuration f state = (\duration' -> state { _treeDuration = duration'}) <$> f (_treeDuration state)

helperInc :: Lens' Constants (HelperInc (Helpers Integer))
helperInc f state = (\inc' -> state { _helperInc = inc'}) <$> f (_helperInc state)

prices :: Lens' Config Prices
prices f state = (\prices' -> state { _prices = prices'}) <$> f (_prices state)

advancedHelperPrice :: Lens' Prices (AdvancedHelperPrice (Paperclips Integer))
advancedHelperPrice f state = (\price' -> state { _advancedHelperPrice = price'}) <$> f (_advancedHelperPrice state)

helperPrices :: Lens' Prices (HelperPrice Integer)
helperPrices f state = (\helperPrice' -> state { _helperPrice = helperPrice'}) <$> f (_helperPrice state)

treePrice :: Lens' Prices (TreePrice Integer)
treePrice f state = (\treePrice' -> state { _treePrice = treePrice'}) <$> f (_treePrice state)

progPrice :: Lens' Prices (ProgPrice Integer)
progPrice f state = (\price' -> state { _progPrice = price'}) <$> f (_progPrice state)

treeSeedPrice :: Lens' Prices TreeSeedPrice
treeSeedPrice f state = (\price' -> state { _treeSeedPrice = price'}) <$> f (_treeSeedPrice state)

actions :: Lens' MyState [Action]
actions f state = (\actions' -> state { _actions = actions'}) <$> f (_actions state)

errorLog :: Lens' MyState [ErrorLogLine]
errorLog f state = (\errorLog' -> state { _errorLog = errorLog'}) <$> f (_errorLog state)

resources :: Lens' MyState Resources
resources f state = (\resources' -> state { _resources = resources'}) <$> f (_resources state)

elements :: Lens' Resources Elements
elements f state = (\elements' -> state { _elements = elements'}) <$> f (_elements state)

paperclips :: Lens' Elements (Paperclips Integer)
paperclips f state = (\paperclips' -> state { _paperclips = paperclips'}) <$> f (_paperclips state)

helpers :: Lens' Elements (Helpers Integer)
helpers f state = (\helpers' -> state { _helpers = helpers'}) <$> f (_helpers state)

storage :: Lens' Resources (Storage (Paperclips Integer))
storage f state = (\storage' -> state { _storage = storage'}) <$> f (_storage state)

water :: Lens' Elements (Water Integer)
water f state = (\water' -> state { _water = water'}) <$> f (_water state)

waterTank :: Lens' Resources (WaterTank Integer)
waterTank f state = (\tank' -> state { _waterTank = tank'}) <$> f (_waterTank state)

trees :: Lens' Elements (Trees Integer)
trees f state = (\trees' -> state { _trees = trees'}) <$> f (_trees state)

treeSeeds :: Lens' Elements (TreeSeeds Integer)
treeSeeds f state = (\treeSeeds' -> state { _treeSeeds = treeSeeds'}) <$> f (_treeSeeds state)

progs :: Lens' (TreeSeeds Integer) [Prog Integer]
progs f state = (\treeSeeds' -> TreeSeeds treeSeeds') <$> f (unTreeSeeds state )

researchAreas :: Lens' MyState ResearchAreas
researchAreas f state = (\areas' -> state { _researchAreas = areas'}) <$> f (_researchAreas state)

researchCompProgress :: Lens' ResearchComp ResearchProgress
researchCompProgress f state = (\progress' -> state { _researchCompProgress = progress'}) <$> f (_researchCompProgress state)

advancedHelperResearch :: Lens' ResearchAreas ResearchComp
advancedHelperResearch f state = (\a -> state { _advancedHelperResearch = a}) <$> f (_advancedHelperResearch state)

researchCompDuration :: Lens' ResearchComp Duration
researchCompDuration f state = (\duration -> state { _researchCompDuration = duration}) <$> f (_researchCompDuration state)

seconds :: Lens' MyState Seconds
seconds f state = (\seconds' -> state { _seconds = seconds'}) <$> f (_seconds state)

wood :: Lens' Elements (Wood Integer)
wood f state = (\wood' -> state { _wood = wood'}) <$> f (_wood state)

isStarted :: Lens' MyState IsStarted
isStarted f state = (\isStarted' -> state { _isStarted = isStarted'}) <$> f (_isStarted state)
