module HW1.T4 where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ st Leaf = st
tfoldr op st (Branch _ l x r) = tfoldr op (op x (tfoldr op st r)) l