module HW0.T6 where

import HW0.T1
import Data.Char

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
a_whnf :: (Either [Char] b1, Either [Char] b2)
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

b :: [Bool]
b = map isSpace "Hello, World"
b_whnf :: [Bool]
b_whnf = isSpace 'H' : map isSpace "ello, World"

c :: [Char]
c = if 1 > 0 || error "X" then "Y" else "Z"
c_whnf :: [Char]
c_whnf = ['Y']