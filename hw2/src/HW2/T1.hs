module HW2.T1 where

data Option a = None | Some a

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption g (Some x) = Some $ g x


data Pair a = P a a

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair g (P x y) = P (g x) (g y)


data Quad a = Q a a a a

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad g (Q x y z w) = Q (g x) (g y) (g z) (g w)


infix 0 :#
data Annotated e a = a :# e

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated g (x :# y) = g x :# y


data Except e a = Error e | Success a

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept g (Success x) = Success $ g x
mapExcept _ (Error exc) = Error exc


data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised g (Low x) = Low $ g x
mapPrioritised g (Medium x) = Medium $ g x
mapPrioritised g (High x) = High $ g x


infixr 5 :>
data Stream a = a :> Stream a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream g (x :> stream) = (g x) :> mapStream g stream 


infixr 5 :.
data List a = Nil | a :. List a

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList g (x :. rest) = g x :. mapList g rest


newtype Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F $ f <$> g 

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree g (Branch left x right) = 
    Branch (mapTree g left) (g x) (mapTree g right)
