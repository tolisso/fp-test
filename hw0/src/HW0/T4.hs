module HW0.T4 where
import Data.Function (fix)
import GHC.Natural

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' = fix $ \req f x -> 
    if null x 
        then [] 
        else f (head x) : req f (tail x)

fib :: Natural -> Natural
fib n = fix (\req i a b -> 
    if i == n 
        then a
        else req (i + 1) b (a + b)) 0 0 1

fac :: Natural -> Natural
fac = fix $ \req n ->
    if n == 0
        then 1
        else n * req (n - 1)