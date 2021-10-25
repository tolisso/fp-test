module HW1.T5 where
import Data.List.NonEmpty

addOnTop :: a -> NonEmpty [a] -> NonEmpty [a]
addOnTop a (b :| c) = (a : b) :| c

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn _ [] = [] :| []
splitOn sep (a : b) = (if sep == a then cons [] else addOnTop a)
    $ splitOn sep b

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (a :| b) = a ++ foldr (\c d -> sep : c ++ d) [] b

-- joinWith sep (a :| b) = a ++ foldr ((++) <$> (++) [sep]) [] b