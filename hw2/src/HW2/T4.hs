module HW2.T4 where

import HW2.T1
import qualified Control.Monad


data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S runf) = S $ (mapAnnotated f) <$> runf

wrapState :: a -> State s a
wrapState x = S $ (:#) x  

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s -> run (f s) where
    run (x :# s) = runS x s


modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
    fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving Show


data Expr = Val Double | Op (Prim Expr) deriving Show

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
    x / y = Op (Div x y)
    fromRational x = Val (fromRational x) 

evalBinOperation :: Expr -> Expr -> (Double -> Double -> Double) -> 
    (Double -> Double -> Prim Double) -> State [Prim Double] Double
evalBinOperation x y f op = (do 
    a <- (eval x); 
    b <- (eval y); 
    modifyState ((:) (op a b)); 
    return $ f a b)

evalUnOperation :: Expr -> (Double -> Double) -> 
    (Double -> Prim Double) -> State [Prim Double] Double
evalUnOperation x f op = (do 
    a <- (eval x); 
    modifyState ((:) (op a)); 
    return $ f a)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = wrapState x
eval (Op (Add x y)) = evalBinOperation x y (+) Add
eval (Op (Sub x y)) = evalBinOperation x y (-) Sub
eval (Op (Mul x y)) = evalBinOperation x y (*) Mul
eval (Op (Div x y)) = evalBinOperation x y (/) Div
eval (Op (Abs x)) = evalUnOperation x abs Abs
eval (Op (Sgn x)) = evalUnOperation x signum Abs

