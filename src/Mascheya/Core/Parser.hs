{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Mascheya.Core.Parser where

import qualified Data.Char as Char
import Prelude hiding (repeat)
import Mascheya.Core.Ast.Source 
import Mascheya.Core.Result (Result, parseError, ParseError (Invalid, Eof, Expected))
import qualified Mascheya.Core.Lexemes as Lexemes
import Control.Applicative (Alternative (empty, (<|>)))
import qualified Mascheya.Core.Result as Result
import Control.Monad (join)
import GHC.Float (double2Float)
import Control.Monad.Reader (ReaderT(..))

type Step a = Parse (a, String)
newtype Parser a = Parser { run :: Parse a }
type Parse a = String -> Result a

newtype ParComb a = ParComb (Step a)

parse :: Step a -> Parse a
parse parser = (>>= handle) . parser
  where
    handle (val, "") = return val
    handle (_, rest) = parseError $ Invalid "characters" rest

parseLit :: Parse Literal
parseLit = run $ intLitParser <|> doubleLitParser <|> floatLitParser

intLitParser :: Parser Literal
intLitParser = IntLit <$> intParser

intParser :: Parser SInt
intParser = Parser parseInt

doubleLitParser :: Parser Literal
doubleLitParser = DoubleLit <$> doubleParser

doubleParser :: Parser SDouble
doubleParser = Parser parseDouble

parseInt :: Parse SInt
parseInt = (SInt . read <$>) . parse int 

parseDouble :: Parse SDouble
parseDouble = (SDouble . read <$>) . parse double

parseFloat :: Parse SFloat
parseFloat = (SFloat . read <$>) . parse float

floatLitParser :: Parser Literal
floatLitParser = FloatLit <$> floatParser

floatParser :: Parser SFloat
floatParser = Parser parseFloat

-- parseFloat :: Parse SFloat
-- parseFloat = (SFloat . read <$>) . parse float

charParser :: Char -> Parser Char
charParser = Parser . parse . char 

char :: Char -> Step Char
char c = (>>= handle) . advance
  where 
    handle (h, rest) | h == c = return (h, rest)
      | otherwise = parseError $ Expected [c] [h]

int :: Step String
int source = advance source >>= \(c, rest) ->
  if c == '-' 
  then (\(n, rest2) -> (c : n, rest2)) <$> nat rest 
  else nat source

double :: Step String
double src = int src >>= \intResult@(whole, rest1) ->
  case advance rest1 of
    Left _ -> return intResult
    Right (c, rest2) -> 
      if c == Lexemes.dot 
      then (\(fractional, rest3) -> (whole ++ (c : fractional), rest3)) <$> nat rest2
      else return intResult

-- float :: Step String
-- float src = do
--   doubleRes@(doubleStr, rest) <- double src
--   (c, rest2) <- advance rest
--   if c == Lexemes.floatSuffix 
--   then return (doubleStr, rest2)
--   else return doubleRes

float :: Step String
float = ((\((d, _), r) -> (d, r)) <$>) . (double <&> char 'f')

nat :: Step String
nat = repeat digit

digit :: Step Char
digit src = peek src >>= \c ->
  if Char.isDigit c then advance src else parseError $ Invalid "digit" src

repeat :: Step Char -> Step String
repeat parser src = parser src >>= \(c, cs) -> case repeat parser cs of
  Left _ -> return (c : [], cs)
  Right (cs', css) -> return (c : cs', css)  

peek :: Parse Char
peek "" = parseError Eof
peek (c : _) = return c

advance :: Step Char
advance [] = parseError Eof
advance (s : ss) = return (s, ss)

(<&>) :: Step a -> Step b -> Step (a, b)
(<&>) pa pb = \src -> do
  (a, rest) <- pa src
  (b, rest1) <- pb rest
  return ((a, b), rest1)
  
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const $ parseError Eof

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) pA pB = Parser {
    run = \src -> case (run pA) src of
      Left _ -> (run pB) src
      success -> success
  } 

instance Applicative Parser where
  pure :: a -> Parser a
  pure val = Parser $ const $ Result.succeed val 
  
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) parseF parseA = do
    f <- parseF
    a <- parseA
    return $ f a

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser runA) = Parser {
    run = (f <$>) . runA
  }

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) parseA = join . (<$> parseA)
