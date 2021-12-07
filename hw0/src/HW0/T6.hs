module HW0.T6 where

import HW0.T1
import Data.Char

a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

b = map isSpace "Hello, World"
b_whnf = isSpace 'H' : map isSpace "ello, World"

c = if 1 > 0 || error "X" then "Y" else "Z"
c_whnf = "Y"