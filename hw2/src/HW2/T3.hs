module HW2.T3 where

import HW2.T1

joinOption :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some None) = None
joinOption (Some (Some x)) = Some x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success x)) = Success x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# ei) :# eo) = x :# eo <> ei;

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. rest) = joinList rest
joinList ((x :. inrest) :. rest) = x :. joinList (inrest :. rest)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ getF <*> f where 
    getF :: i -> (Fun i a) -> a
    getF x (F g) = g x