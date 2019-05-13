{-# LANGUAGE OverloadedStrings #-}

module Lib (module Lib, module Mod) where

import Control.Lens
import Data.Text (concat, pack)

import Mod

nextTick :: MyState -> MyState
nextTick = addSecond . helperWork

helperWork :: MyState -> MyState
helperWork state =
  let h = view (resources.helpers) state
  in over (resources.paperclips) (addHelperWork h) state

addHelperWork :: Helpers -> Paperclips -> Paperclips
addHelperWork h p = Paperclips $ (unPaperclips p) + (unHelpers h) * 2

addSecond :: MyState -> MyState
addSecond = over seconds succ

createPC :: MyState -> MyState
createPC = over (resources.paperclips) succ

setO3 :: ASetter' s a -> ASetter' s b -> ASetter' s c -> s -> (a, b, c) -> s
setO3 f1 f2 f3 s (a, b, c) = set f1 a $ set f2 b $ set f3 c s

buyHelper :: MyState -> MyState
buyHelper state =
  setOutput state $ buyHelper' $ getInput state
  where
    getInput = getI5
      seconds
      (config.prices.helperPrices)
      (resources.paperclips)
      (resources.helpers)
      errorLog
    setOutput = setO3 (resources.paperclips) (resources.helpers) errorLog

buyHelper'' :: Seconds -> HelperPrice -> Paperclips -> Helpers -> [ErrorLogLine] -> Either [ErrorLogLine] (Helpers, Paperclips)
buyHelper'' s price p helpers log =
  if unHelperPrice price > p
    then Left $ addToErrorLog (lineNeedMorePaperclips s) log
    else Right (succ helpers, decPaperclipsWith price p)

getI5
  :: Getting a s a
  -> Getting b s b
  -> Getting c s c
  -> Getting d s d
  -> Getting e s e
  -> s
  -> (a, b, c, d, e)
getI5 f1 f2 f3 f4 f5 s =
  ( view f1 s
  , view f2 s
  , view f3 s
  , view f4 s
  , view f5 s )

buyHelper' :: (Seconds, HelperPrice, Paperclips, Helpers, [ErrorLogLine]) -> (Paperclips, Helpers, [ErrorLogLine])
buyHelper' (s, hp, pc, helpers, errs )= case buyHelper'' s hp pc helpers errs of
  Left errs' -> (pc, helpers, errs')
  Right (hp', pc') -> (pc', hp', errs)

decPaperclipsWith :: HelperPrice -> Paperclips -> Paperclips
decPaperclipsWith price paperclips = paperclips - (unHelperPrice price)

addToErrorLog :: ErrorLogLine -> [ErrorLogLine] -> [ErrorLogLine]
addToErrorLog new existing = existing ++ [new]

lineNeedMorePaperclips :: Seconds -> ErrorLogLine
lineNeedMorePaperclips s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more paperclips."]

plantASeed :: MyState -> MyState
plantASeed state =
  let seeds = view (resources.treeSeeds) state
      s' = view seconds state
    in
      if 1 > (unTreeSeeds seeds)
        then over errorLog (addToErrorLog (lineNeedMoreSeeds s')) state
        else over (resources.trees) succ $ over (resources.treeSeeds) pred state

lineNeedMoreSeeds :: Seconds -> ErrorLogLine
lineNeedMoreSeeds s = ErrorLogLine $ Data.Text.concat ["Tick ", pack (show s), ": You need more seeds."]

setStarted :: MyState -> MyState
setStarted = over isStarted (const $ IsStarted True)

initialPrices :: Prices
initialPrices = Prices (HelperPrice $ Paperclips 10) (TreePrice $ TreeSeeds 1)

getInitialState :: MyState
getInitialState = MyState (Config initialPrices) [] [] (Resources 0 0 0 10) 0 (IsStarted False)