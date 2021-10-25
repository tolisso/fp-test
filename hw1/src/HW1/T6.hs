module HW1.T6 where
import Data.Maybe
import Data.Foldable

mcat :: Monoid a => [Maybe a] -> a
-- mcat x = mconcat (catMaybes x)

mcat x = fromMaybe mempty (fold x)

concatREather :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
concatREather (Left x) (a, b) = (x <> a, b) 
concatREather (Right x) (a, b) = (a, x <> b) 

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr concatREather (mempty, mempty) 

