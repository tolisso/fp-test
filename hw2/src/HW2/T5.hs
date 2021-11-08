module HW2.T5 where

import HW2.T1
import HW2.T2 
import HW2.T4 hiding (eval, evalBinOperation, evalUnOperation) 
import GHC.Base (Functor)
import qualified Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ (mapExcept (mapAnnotated f)) <$> g

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (\s -> wrapExcept (x :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \v -> getFromExcept $ runES es v where
    getFromExcept (Error s) = Error s
    getFromExcept (Success (x :# inv)) = runES x inv

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState ex = ES (\_ -> Error ex)

-- defining monad instance
instance Functor (ExceptState e s) where
    fmap = mapExceptState

instance Applicative (ExceptState e s) where
    pure x = wrapExceptState x
    (<*>) = Control.Monad.ap 

instance Monad (ExceptState e s) where
    x >>= y = joinExceptState $ fmap y x

-- defining eval method
data EvaluationError = DivideByZero deriving Show

evalBinOperation :: 
    Expr ->                                 -- first expression argument 
    Expr ->                                 -- second expression argument
    (Double -> Double -> Prim Double) ->    -- new state array element getter
                                            -- arithmetic operation result monad
    (Double -> Double -> 
        ExceptState EvaluationError [Prim Double] Double) ->         
    ExceptState EvaluationError [Prim Double] Double

evalBinOperation x y op f = (do 
    a <- (eval x); 
    b <- (eval y); 
    modifyExceptState ((:) (op a b)); 
    f a b)

evalUnOperation :: 
    Expr ->                                 -- expression argument 
    (Double -> Prim Double) ->              -- new state array element getter
                                            -- arithmetic operation result monad
    (Double -> ExceptState EvaluationError [Prim Double] Double) ->
    ExceptState EvaluationError [Prim Double] Double

evalUnOperation x op f = (do 
    a <- (eval x); 
    modifyExceptState ((:) (op a)); 
    f a)

mkBinary ::
    (Double -> Double -> Double) ->         -- double operation
    Double ->                               -- first arg
    Double ->                               -- second arg
    ExceptState EvaluationError [Prim Double] Double
mkBinary f x y = return $ f x y

mkUnary ::
    (Double -> Double) ->                   -- double operation
    Double ->                               -- arg
    ExceptState EvaluationError [Prim Double] Double
mkUnary f x = return $ f x

divWithExc :: (Double -> Double -> 
    ExceptState EvaluationError [Prim Double] Double)
divWithExc _ 0 = ES $ \_ -> Error DivideByZero
divWithExc x y = return $ x / y

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = wrapExceptState x
eval (Op (Add x y)) = evalBinOperation x y Add $ mkBinary (+)
eval (Op (Sub x y)) = evalBinOperation x y Sub $ mkBinary (-)
eval (Op (Mul x y)) = evalBinOperation x y Mul $ mkBinary (*)
eval (Op (Div x y)) = evalBinOperation x y Div $ divWithExc
eval (Op (Abs x)) = evalUnOperation x Abs $ mkUnary abs
eval (Op (Sgn x)) = evalUnOperation x Sgn $ mkUnary signum