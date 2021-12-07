module HW0.T5 where
import GHC.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f x = f (n f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus nx ny f x = nx f (ny f x)

nmult nx ny f x = nx (ny f) x

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural x = ns (nFromNatural (x - 1))

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0