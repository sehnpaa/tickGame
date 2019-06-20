module Utils where

import           Control.Applicative            ( liftA2 )

twoLevels
    :: (Applicative f, Applicative g)
    => (a -> b -> c)
    -> f (g a)
    -> f (g b)
    -> f (g c)
twoLevels = liftA2 . liftA2

changeFirst :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
changeFirst _ _ []       = []
changeFirst f g (x : xs) = if f x then g x : xs else x : changeFirst f g xs

