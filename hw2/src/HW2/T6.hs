{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HW2.T6 where
import HW2.T1(Except(Error, Success), Annotated((:#)))
import HW2.T4
import HW2.T5
import Numeric.Natural
import Control.Applicative(Alternative(..), Applicative(..), many, optional, some)
import Control.Monad(MonadPlus, mfilter, msum, void)
import Data.Char
import Data.Scientific
import Prelude
import GHC.Real

data ParseError = ErrorAtPos Natural
  deriving Show
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)


runP :: Parser a -> String -> Except ParseError a
runP (P ex) s = f $ runES ex (0, s)
  where
    f (Success (res :# _)) = Success res
    f (Error res) = Error res

pChar :: Parser Char
pChar = P $ ES (\(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs)))

isError :: Except ParseError a -> Bool
isError (Error _) = True
isError (Success _) = False

parseError :: Parser a
parseError = P $ ES (\(pos, _) -> Error (ErrorAtPos pos))

instance Alternative Parser where
  empty = parseError
  (P a) <|> (P b) = P $ ES (\(pos, s) ->
    let aboba = runES a (pos, s)
    in if isError aboba then runES b (pos, s)
      else aboba)

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = P $ ES (\(pos, s) ->
  case s of
    [] -> Success (() :# (0, ""))
    _ -> Error (ErrorAtPos pos))

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = mfilter f pChar

pSpaces :: Parser () 
pSpaces = void(many (satisfy isSpace))

pDigit :: Parser Char
pDigit = satisfy isDigit

pChr :: Char -> Parser Char
pChr c = satisfy (== c)

pDigits :: Parser String
pDigits = some pDigit

pInteger :: Parser (Integer, Int)
pInteger = foldl (\x y -> toInteger x *  10 + toInteger (ord y - ord '0')) (toInteger 0) <$> pDigits

pDouble :: Parser Double
pDouble = do
  (d1, x) <- pInteger
  pChr '.'
  (d2, y) <- pInteger
  return $ toRealFloat $ (scientific d1 0) + (scientific d2 (-y))

pE :: Parser Expr
pE = do
  pSpaces
  t <- pT
  pE' t

pE' :: Expr -> Parser Expr
pE' expr = do
  pSpaces
  pChr '+'
  t <- pT
  e' <- pE' t
  pure (Op (Add expr e'))
  <|> do
  pSpaces
  pChr '-'
  t <- pT
  e' <- pE' t
  pure (Op (Sub expr e'))
  <|>
  pure expr

pT :: Parser Expr
pT = do
  pSpaces
  f <- pF
  pT' f

pT' :: Expr -> Parser Expr
pT' expr = do
  pSpaces
  pChr '*'
  t <- pT
  e' <- pE' t
  pure (Op (Mul expr e'))
  <|> do
  pSpaces  
  pChr '/'
  t <- pT
  e' <- pE' t
  pure (Op (Div expr e'))
  <|>
  pure expr

pF :: Parser Expr
pF = (Val <$> pDouble) <|> do
  pSpaces
  pChr '('
  pSpaces
  e <- pE
  pSpaces
  pChr ')'
  pure e

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (do 
  pSpaces
  e <- pE
  pSpaces
  pEof
  pure e)
