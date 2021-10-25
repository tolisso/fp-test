module HW1.T7 where

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last x) y = x :+ y
  (<>) (a :+ b) y = a :+ (b <> y)

data Inclusive a b = This a | That b | Both a b 

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
    That a   <> That b   = That $ a <> b
    This a   <> This b   = This $ a <> b
    This a   <> That b   = Both a b
    That b   <> This a   = Both a b
    Both a b <> This c   = Both (a <> c) b
    Both a b <> That c   = Both a (b <> c)
    This c   <> Both a b = Both (c <> a) b
    That c   <> Both a b = Both a (c <> b)
    Both a b <> Both c d = Both (a <> c) (b <> d)

newtype DotString = DS String deriving Show

instance Semigroup DotString where
    (<>) (DS "") b = b
    (<>) a (DS "") = a
    (<>) (DS a) (DS b) = DS $ a ++ "." ++ b 

instance Monoid DotString where
    mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    (<>) (F a) (F b) = F (a . b)

instance Monoid (Fun a) where
    mempty = F id
