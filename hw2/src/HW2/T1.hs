module HW2.T1 where

data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f = box f where
    box :: (a -> b) -> Option a -> Option b 
    box f None = None
    box f (Some x) = Some $ f x


data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f = box f where
    box :: (a -> b) 


data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#
data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>
data List a = Nil | a :. List a

infixr 5 :.
newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
