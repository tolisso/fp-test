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
import Control.Applicative
import GHC.Base (MonadPlus, Monoid)

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
