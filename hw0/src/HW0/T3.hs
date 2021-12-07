module HW0.T3 where

s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

k :: a -> b -> a
k x _ = x

i :: a -> a
i = s k k

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

contract :: (a -> a -> b) -> (a -> b)
contract = s s (k i)

m :: (a -> b -> c) -> (a -> b) -> a -> c
m = s (k s) i

p :: (b1 -> a -> b2 -> c) -> b1 -> (a -> b2) -> a -> c
p = s (k m)

d :: (a -> b2 -> c) -> b1 -> (a -> b2) -> a -> c
d = s (k p) k 

r :: (a1 -> b2 -> c) -> (a2 -> a1 -> b2) -> a2 -> a1 -> c
r = s (k s) d

permute :: (a -> b -> c) -> (b -> a -> c)
permute = s r (k k)