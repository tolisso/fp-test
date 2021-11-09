{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T6 where
import Data.Scientific
import Data.Char (isSpace, isDigit)

import HW2.T1 (
        Except (Error, Success),
        mapExcept,
        Annotated ((:#))
    )
import HW2.T5
import GHC.Natural
import Control.Applicative
import GHC.Base (MonadPlus, Monoid)
import qualified Control.Monad
import HW2.T4

data ParseError = ErrorAtPos Natural 

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
pDefChar ch = pCondChar $ (==) ch

pCondChar :: (Char -> Bool) -> Parser ()
pCondChar test = mkP $ do x <- pChar; check $ test x

check :: Bool -> Parser ()
check b = do
    Control.Monad.unless b $ parseError

pSpaces :: Parser ()
pSpaces = do _ <- many $ pCondChar isSpace; return ()

pSpaceWrap :: Parser a -> Parser a
pSpaceWrap p = do
    x <- p
    pSpaces
    return x

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
pDigit = do
    x <- pChar
    check $ Data.Char.isDigit x
    return $ charToDigit x

pInteger :: Parser (Integer, Int) -- (value, length)
pInteger = do
    x <- some pDigit
    return $ (foldl (\acc val -> acc * 10 + val) 0 x,
        length x)

pDouble :: Parser Double
pDouble = pSpaceWrap $ mkP $ do
    (x, _) <- pInteger
    pDefChar '.'
    (y, er) <- pInteger
    return $ toRealFloat (scientific x 0 + scientific y (-er))

term :: Parser Expr 
term = do {
        x <- pDouble;
        return $ Val x;
    }
    <|>
    do {
        pDefChar '(';
        pSpaces;
        x <- add; 
        pDefChar ')';
        pSpaces;
        return x;
    }

add :: Parser Expr
add = pSpaceWrap $ do {
        x <- mul;
        y <- add';
        return $ y x;
    } 

mul :: Parser Expr
mul = pSpaceWrap $ do {
        x <- term;
        y <- mul';
        return $ y x;
    }

                                        -- parsing [+ x] [* y * ...] - parts
op' :: Parser Expr                      -- first part parser
    -> Parser (Expr -> Expr)            -- second part parser
    -> (Expr -> Expr -> Prim Expr)      -- expression constructor
    -> Char                             -- expression operator
    -> Parser (Expr -> Expr)
op' curT nextT oper opCh = do {
            pDefChar opCh;
            pSpaces;
            x <- curT;
            pSpaces;
            y <- nextT;
            pSpaces;
            return \a -> y $ Op $ oper a x;
        }

add' :: Parser (Expr -> Expr)
add' = opParser Add '+' 
    <|> opParser Sub '-' 
    <|> return id                   
    where
        opParser = op' mul add'  

mul' :: Parser (Expr -> Expr)
mul' = opParser Mul '*' 
    <|> opParser Div '/'
    <|> return id
    where
        opParser = op' term mul'

parseExpr :: String -> Except ParseError Expr
parseExpr str = runP 
    (do {
            pSpaces; 
            x <- add;
            pEof;
            return x;
        }) 
    str