module HW1.T2 where
import GHC.Natural
import Data.Maybe

data N = Z | S N

nplus :: N -> N -> N        -- addition
nplus Z b = b
nplus (S a) b = S $ nplus a b

nmult :: N -> N -> N        -- multiplication
nmult Z _ = Z
nmult (S a) b = nplus b $ nmult a b

nsub :: N -> N -> Maybe N   -- subtraction
nsub a Z = Just a
nsub (S a) (S b) = nsub a b
nsub Z (S _) = Nothing

ncmp :: N -> N -> Ordering  -- comparison
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S a) (S b) = ncmp a b

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

instance Show N where
    show n = show (nToNum n :: Natural)

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ n - 1

nEven, nOdd :: N -> Bool    -- parity checking

nEven Z = True
nEven (S a) = nOdd a

nOdd Z = False
nOdd (S a) = nEven a

ndivmod :: N -> N -> N -> N -> (N, N)
ndivmod Z b modl res = (res, fromJust (nsub b modl))
ndivmod (S a) b (S Z) res = ndivmod a b b (S res)
ndivmod (S a) b (S c) res = ndivmod a b c res
ndivmod _ Z _ _ = error "divisor = zero"
ndivmod _ _ Z _ = error "mod = divisor"

ndiv :: N -> N -> N         -- integer division
ndiv _ Z = error "division by zero" 
ndiv a b = fst $ ndivmod a b b Z

nmod :: N -> N -> N         -- modulo operation
nmod _ Z = error "division by zero" 
nmod a b = snd $ ndivmod a b b Z

