{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T6 where

import HW2.T1 (
        Except (Error, Success),
        mapExcept,
        Annotated ((:#))
    )
import HW2.T5
import GHC.Natural
import Data.Scientific as Scientific
import Control.Applicative
import GHC.Base (MonadPlus, Monoid)
import qualified Data.Char
import qualified Control.Monad
import Data.Foldable
import HW2.T4 (Expr)

data ParseError = ErrorAtPos Natural deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
    deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str =  mapExcept getVal $ runES es (0, str) where
    getVal (x :# _) = x

pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
    case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
    empty = parseError
    (P esx) <|> (P esy) = P $ ES \s
        -> eatherRes (runES esx s) (runES esy s) where
        eatherRes (Error _) y = y
        eatherRes x _ = x

instance Semigroup (Parser a) where
    (<>) = (<|>)

instance Monoid (Parser a) where
    mempty = empty
    mappend = (<|>)

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES \(pos, s) -> case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pDefChar :: Char -> Parser ()
pDefChar ch = mkP$ do x <- pChar; check $ x == ch

check :: Bool -> Parser ()
check b = do
    Control.Monad.unless b $ parseError

mkP :: Parser a -> Parser a
mkP p = p <|> parseError

charToDigit :: Char -> Integer
charToDigit '0' = 0
charToDigit '1' = 1
charToDigit '2' = 2
charToDigit '3' = 3
charToDigit '4' = 4
charToDigit '5' = 5
charToDigit '6' = 6
charToDigit '7' = 7
charToDigit '8' = 8
charToDigit '9' = 9
charToDigit _ = undefined

pDigit :: Parser Integer
pDigit = mkP do
    x <- pChar
    check $ Data.Char.isDigit x
    return $ charToDigit x

pInteger :: Parser (Integer, Int) -- (value, length)
pInteger = mkP do
    x <- some pDigit
    return $ (foldl (\acc val -> acc * 10 + val) 0 x,
        length x)

pDouble :: Parser Double
pDouble = mkP do
    (x, _) <- pInteger
    pDefChar '.'
    (y, er) <- pInteger
    return $ toRealFloat (scientific x 0 + scientific y (-er))

pVal :: [Parser (Double -> Double)] -> Parser Double
pVal rPartPs = mkP do
    x <- pDouble
    lit <- some (fold rPartPs)
    return $ foldl (\acc op -> op acc) x lit

pRight :: (Double -> Double -> Double)
    -> Char
    -> [Parser Double]
    -> Parser (Double -> Double)
pRight op opCh rPartPs = do
    pDefChar opCh
    x <- fold rPartPs
    return $ flip op x

pLit :: Parser Double
pLit = pVal [pLitRight]
pTerm :: Parser Double
pTerm = pVal [pTermRight]

pLitRight :: Parser (Double -> Double)
pLitRight =
    pRight (+) '+' [pDouble, pTerm]
    <|>
    pRight (-) '-' [pDouble, pTerm]

pTermRight :: Parser (Double -> Double)
pTermRight =
    pRight (*) '*' [pDouble]
    <|>
    pRight (/) '/' [pDouble]

parseExpr :: String -> Except ParseError Double
parseExpr str = runP (pLit <|> pTerm) str