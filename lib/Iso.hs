module Iso where

import Control.Lens (Profunctor, iso)

import Mod

advancedHelperPrice :: (Profunctor p, Functor f) => p AdvancedHelperPrice (f AdvancedHelperPrice) -> p Paperclips (f Paperclips)
advancedHelperPrice = iso AdvancedHelperPrice unAdvancedHelperPrice

helperInc :: (Profunctor p, Functor f) => p HelperInc (f HelperInc) -> p Helpers (f Helpers)
helperInc = iso HelperInc unHelperInc

helperPrice :: (Profunctor p, Functor f) => p HelperPrice (f HelperPrice) -> p Paperclips (f Paperclips)
helperPrice = iso HelperPrice unHelperPrice

helpers :: (Profunctor p, Functor f) => p Helpers (f Helpers) -> p Integer (f Integer)
helpers = iso Helpers unHelpers

paperclips :: (Profunctor p, Functor f) => p Paperclips (f Paperclips) -> p Integer (f Integer)
paperclips = iso Paperclips unPaperclips

treePrice :: (Profunctor p, Functor f) => p TreePrice (f TreePrice) -> p Integer (f Integer)
treePrice = iso TreePrice unTreePrice

treeSeeds :: (Profunctor p, Functor f) => p TreeSeeds (f TreeSeeds) -> p [Prog] (f [Prog])
treeSeeds = iso TreeSeeds unTreeSeeds