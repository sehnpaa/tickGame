module Tradeoff where

import           Data.Function
import           Data.List
import           Data.Validation

data Ability = Ability String deriving (Eq, Show)

type Capital a = [(Resource a, a)]

data Asset a = Asset (Capital a) [Ability] deriving (Eq, Show)

data Error a
    = MissingAbilities [Ability]
    | CapitalTooLow (Resource a) a
    | CapitalTooHigh (Resource a) a deriving (Eq, Show)

data Tradeoff a = Tradeoff
    { gains :: Capital a
    , losses :: Capital a
    , achieveAbilities :: [Ability]
    , prerequisites :: [Ability] } deriving Show

data Resource a = Resource
    { resourceName :: String
    , resourceLowerLimit :: Maybe a
    , resourceUpperLimit :: Maybe a } deriving (Eq, Ord, Show)

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace x t = fmap (\c -> if c == x then t else c)

mergeRA :: (Eq a, Num a) => Capital a -> (Resource a, a) -> Capital a
mergeRA b (r, a) = case find (\(r', _) -> r == r') b of
    Nothing          -> (r, negate a) : b
    Just ra@(r', a') -> replace ra (r', a' - a) b

removeCapital :: (Eq a, Num a) => Capital a -> Capital a -> Capital a
removeCapital = foldl mergeRA

addCapital :: (Eq a, Num a) => Capital a -> Capital a -> Capital a
addCapital = foldl (upsert (+))

upsert :: Eq a => (b -> b -> b) -> [(a, b)] -> (a, b) -> [(a, b)]
upsert _ [] (a, b) = [(a, b)]
upsert bbb ((x, y) : xs) (a, b) =
    if x == a then (x, bbb y b) : xs else (x, y) : upsert bbb xs (a, b)

addAbilities :: [Ability] -> [Ability] -> [Ability]
addAbilities as bs = nub $ as ++ bs

validateAbilities :: [Ability] -> Tradeoff a -> Validation [Error a] [Ability]
validateAbilities as t =
    let xs = prerequisites t \\ as
    in  if xs == []
        then Success (addAbilities as (achieveAbilities t))
        else Failure [MissingAbilities xs]

validateCapitalLowerLimitSingle
    :: (Ord a) => (Resource a, a) -> Validation [Error a] [(Resource a, a)]
validateCapitalLowerLimitSingle (r, a) =
    let mayLimit = resourceLowerLimit r in
    case mayLimit of
    Nothing -> Success [(r,a)]
    Just limit -> if a < limit then Failure [CapitalTooLow r a] else Success [(r, a)]

validateCapitalUpperLimitSingle
    :: (Ord a) => (Resource a, a) -> Validation [Error a] [(Resource a, a)]
validateCapitalUpperLimitSingle (r, a) =
    let mayLimit = resourceUpperLimit r in
    case mayLimit of
    Nothing -> Success [(r,a)]
    Just limit -> if a > limit then Failure [CapitalTooHigh r a] else Success [(r, a)]

rules :: (Ord a) => [(Resource a, a) -> Validation [Error a] [(Resource a, a)]]
rules = [validateCapitalUpperLimitSingle, validateCapitalLowerLimitSingle]

validateRA :: (Eq a, Ord a) => (Resource a, a) -> Validation [Error a] (Capital a)
validateRA = mergeSuccessConjunction . nub . (<$> rules) . (&)

validateTransfer :: (Ord a) => Capital a -> Validation [Error a] (Capital a)
validateTransfer = mergeSuccessConjunction . map validateRA

mergeSuccessDisjunction
    :: [Validation [Error a] (Capital a)] -> Validation [Error a] (Capital a)
mergeSuccessDisjunction = foldr mappendDisjunction (Success [])

mergeSuccessConjunction
    :: [Validation [Error a] (Capital a)] -> Validation [Error a] (Capital a)
mergeSuccessConjunction = foldr mappendConjunction (Success [])

-- For Success, require that any is Success
mappendDisjunction
    :: Validation [Error a] (Capital a)
    -> Validation [Error a] (Capital a)
    -> Validation [Error a] (Capital a)
mappendDisjunction (Failure a) (Failure b) = Failure $ mappend a b
mappendDisjunction (Failure _) (Success b) = Success b
mappendDisjunction (Success a) (Failure _) = Success a
mappendDisjunction (Success a) (Success b) = Success $ mappend a b

-- For Success, require that none is Failure
mappendConjunction
    :: Validation [Error a] (Capital a)
    -> Validation [Error a] (Capital a)
    -> Validation [Error a] (Capital a)
mappendConjunction (Failure a) (Failure b) = Failure $ mappend a b
mappendConjunction (Failure a) (Success _) = Failure a
mappendConjunction (Success _) (Failure b) = Failure b
mappendConjunction (Success a) (Success b) = Success $ mappend a b

tryTransfer :: (Eq a, Num a) => Capital a -> Tradeoff a -> Capital a
tryTransfer rs t = removeCapital (addCapital rs (gains t)) (losses t)

validateCapital :: (Num a, Ord a) => Capital a -> Tradeoff a -> Validation [Error a] (Capital a)
validateCapital rs t = validateTransfer $ tryTransfer rs t

trade :: (Num a, Ord a) => Asset a -> Tradeoff a -> Validation [Error a] (Asset a)
trade (Asset c as) t = Asset <$> validateCapital c t <*> validateAbilities as t
