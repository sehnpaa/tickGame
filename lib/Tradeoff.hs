module Tradeoff where

import           Data.Function
import           Data.List
import           Data.Validation

data Ability = Ability String deriving (Eq, Show)

type Amount = Integer
type Capital = [(Resource, Amount)]

data Asset = Asset Capital [Ability] deriving (Eq, Show)

data Error
    = MissingAbilities [Ability]
    | CapitalTooLow Resource Amount
    | CapitalTooHigh Resource Amount deriving (Eq, Show)

data Tradeoff = Tradeoff
    { gains :: Capital
    , losses :: Capital
    , achieveAbilities :: [Ability]
    , prerequisites :: [Ability] } deriving Show

data Resource = Resource
    { resourceName :: String
    , resourceLowerLimit :: Integer
    , resourceUpperLimit :: Integer } deriving (Eq, Ord, Show)

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace x t = fmap (\c -> if c == x then t else c)

mergeRA :: Capital -> (Resource, Amount) -> Capital
mergeRA b (r, a) = case find (\(r', _) -> r == r') b of
    Nothing          -> (r, negate a) : b
    Just ra@(r', a') -> replace ra (r', a' - a) b

removeCapital :: Capital -> Capital -> Capital
removeCapital = foldl mergeRA

addCapital :: Capital -> Capital -> Capital
addCapital = foldl (upsert (+))

upsert :: Eq a => (b -> b -> b) -> [(a, b)] -> (a, b) -> [(a, b)]
upsert _ [] (a, b) = [(a, b)]
upsert bbb ((x, y) : xs) (a, b) =
    if x == a then (x, bbb y b) : xs else (x, y) : upsert bbb xs (a, b)

addAbilities :: [Ability] -> [Ability] -> [Ability]
addAbilities as bs = nub $ as ++ bs

validateAbilities :: [Ability] -> Tradeoff -> Validation [Error] [Ability]
validateAbilities as t =
    let xs = prerequisites t \\ as
    in  if xs == []
            then Success (addAbilities as (achieveAbilities t))
            else Failure [MissingAbilities xs]

validateCapitalLowerLimitSingle
    :: (Resource, Amount) -> Validation [Error] [(Resource, Amount)]
validateCapitalLowerLimitSingle (r, a) =
    let limit = resourceLowerLimit r
    in  if a < limit then Failure [CapitalTooLow r a] else Success [(r, a)]

validateCapitalUpperLimitSingle
    :: (Resource, Amount) -> Validation [Error] [(Resource, Amount)]
validateCapitalUpperLimitSingle (r, a) =
    let limit = resourceUpperLimit r
    in  if a > limit then Failure [CapitalTooHigh r a] else Success [(r, a)]

rules :: [(Resource, Amount) -> Validation [Error] [(Resource, Amount)]]
rules = [validateCapitalUpperLimitSingle, validateCapitalLowerLimitSingle]

validateRA :: (Resource, Amount) -> Validation [Error] Capital
validateRA = mergeSuccessConjunction . nub . (<$> rules) . (&)

validateTransfer :: Capital -> Validation [Error] Capital
validateTransfer = mergeSuccessConjunction . map validateRA

mergeSuccessConjunction
    :: [Validation [Error] Capital] -> Validation [Error] Capital
mergeSuccessConjunction = foldr mappend2 (Success [])

-- For Success, require that none is Failure
mappend2
    :: Validation [Error] Capital
    -> Validation [Error] Capital
    -> Validation [Error] Capital
mappend2 (Failure a) (Failure b) = Failure $ mappend a b
mappend2 (Failure a) (Success _) = Failure a
mappend2 (Success _) (Failure b) = Failure b
mappend2 (Success a) (Success b) = Success $ mappend a b

tryTransfer :: Capital -> Tradeoff -> Capital
tryTransfer rs t = removeCapital (addCapital rs (gains t)) (losses t)

validateCapital :: Capital -> Tradeoff -> Validation [Error] Capital
validateCapital rs t = validateTransfer $ tryTransfer rs t

trade :: Asset -> Tradeoff -> Validation [Error] Asset
trade (Asset c as) t = Asset <$> validateCapital c t <*> validateAbilities as t
