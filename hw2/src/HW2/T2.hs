{-# LANGUAGE TupleSections #-}
module HW2.T2 where 

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (_, None) = None
distOption (None, _) = None
distOption (Some x, Some y) = Some (x, y)

wrapOption :: a -> Option a
wrapOption = Some


distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P x1 x2, P y1 y2) = P (x1, y1) (x2, y2)

wrapPair :: a -> Pair a
wrapPair a = P a a


distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q x1 x2 x3 x4, Q y1 y2 y3 y4) = 
    Q (x1, y1) (x2, y2) (x3, y3) (x4, y4)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x


distAnnotated :: Semigroup e 
    => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# e1 <> e2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated x = x :# mempty


distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success x, Success y) = Success (x, y)

wrapExcept :: a -> Except e a
wrapExcept = Success


distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low x, p) = mapPrioritised (x,) p
distPrioritised (p, Low y) = mapPrioritised (,y) p
distPrioritised (Medium x, p) = mapPrioritised (x,) p
distPrioritised (p, Medium y) = mapPrioritised (,y) p
distPrioritised (High x, High y) = High (x, y)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised x = Low x


distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x :> xtail, y :> ytail) = (x, y) :> distStream (xtail, ytail)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x


distList :: (List a, List b) -> List (a, b)
distList (f, s) = distList' f s where
    distList' Nil _ = Nil
    distList' (_ :. xtail) Nil = distList' xtail s
    distList' (x :. xtail)  (y :. ytail) = 
        (x, y) :. distList' (x :. xtail) ytail

wrapList :: a -> List a
wrapList x = x :. Nil


distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapFun :: a -> Fun i a
wrapFun x = F (const x)