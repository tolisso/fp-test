module Main where

-------------------------
-- Practice 1
-------------------------


------------------------------------------
-- Implement sorting of number sequences
--
-- You should implement three different
-- sorting algorithms.
--
-- Provide implementation in `solve` function.
-- Use `words` function to split whole input to words
-- and `readInt` to convert a word to an integer.
--
-- Your program will be given a sequence
-- of integers (written to stdin).
-- First integer takes value from range [0..2] and
-- signifies sorting algorithm to use.
-- Rest of integers is a sequence that is to be sorted.
--
-- You can compile and run your program with
-- `cabal run practice1`.
--
-------------------------------


main = interact solve

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

solve :: String -> String
solve s = "Hello " ++ s ++ "!"
