module Utils where

import           Data.Bifoldable                ( Bifoldable
                                                , bifoldMap
                                                )

changeFirst :: Eq a => (a -> Bool) -> (a -> a) -> [a] -> [a]
changeFirst _ _ []       = []
changeFirst f g (x : xs) = if f x then g x : xs else x : changeFirst f g xs

singleton :: a -> [a]
singleton a = [a]

withError :: Bifoldable p => (a1 -> a2) -> (b -> [a2]) -> p a1 b -> [a2]
withError errF = bifoldMap (singleton . errF)

withExtendedError
  :: Bifoldable p
  => (t1 -> a)
  -> (t2 -> [a])
  -> (b -> [a])
  -> p (t1, t2) b
  -> [a]
withExtendedError errF f = bifoldMap (\(err, a) -> errF err : f a)

