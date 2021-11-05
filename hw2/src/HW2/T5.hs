module HW2.T5 where

import HW2.T1
import HW2.T2 
import HW2.T4 

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ (mapExcept (mapAnnotated f)) <$> g

wrapExceptState :: a -> ExceptState e s a
wrapExceptState x = ES (\s -> wrapExcept (x :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \v -> runES (getVal f v) () where
    getVal (Error s) = Error s
    getVal (Success (x :# inv)) = runES x inv

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState ex = ES (\_ -> Error ex)
