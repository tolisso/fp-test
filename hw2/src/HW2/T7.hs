module HW2.T7 where
import HW2.T6
import qualified Control.Monad
import qualified Data.Char
import HW2.T4
import HW2.T1
import Control.Applicative
import Data.Scientific

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
pDigit = mkP $ do
    x <- pChar
    check $ Data.Char.isDigit x
    return $ charToDigit x

pInteger :: Parser (Integer, Int) -- (value, length)
pInteger = mkP $ do
    x <- some pDigit
    return $ (foldl (\acc val -> acc * 10 + val) 0 x,
        length x)

pDouble :: Parser Double
pDouble = mkP $ do
    (x, _) <- pInteger
    pDefChar '.'
    (y, er) <- pInteger
    return $ toRealFloat (scientific x 0 + scientific y (-er))

term :: Parser Expr 
term = do 
    x <- pDouble
    return $ Val x

add :: Parser Expr
add = do {
        x <- mul;
        y <- add';
        return $ y x;
    } 

mul :: Parser Expr
mul = do {
        x <- term;
        y <- mul';
        return $ y x;
    }

add' :: Parser (Expr -> Expr)
add' = 
    do {
        pDefChar '+';
        x <- mul;
        y <- add';
        return $ \a -> Op $ Add a (y x);
    } 
    <|>
    return id

mul' :: Parser (Expr -> Expr)
mul' = 
    do {
        pDefChar '*';
        x <- term;
        y <- mul';
        return $ \a -> Op $ Mul a (y x);
    } 
    <|>
    return id

parseExpr :: String -> Except ParseError Expr
parseExpr str = runP add str