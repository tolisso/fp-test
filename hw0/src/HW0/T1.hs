{-# LANGUAGE TypeOperators #-}
module HW0.T1 where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left x) = (Left x, Left x)
distrib (Right (x, y)) = (Right x, Right y)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso to from where
    to (x, (y, z)) = ((x, y), z)
    from ((x, y), z) = (x, (y, z))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso to from where
    to (Left x) = Left (Left x)
    to (Right (Left y)) = Left (Right y)
    to (Right (Right z)) = Right z

    from (Left (Left x)) = Left x
    from (Left (Right y)) = Right (Left y)
    from (Right z) = Right (Right z)