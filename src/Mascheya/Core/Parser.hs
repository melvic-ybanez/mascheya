module Mascheya.Core.Parser where

import qualified Data.Char as Char
import Prelude hiding (repeat)
import Mascheya.Core.Ast.Source (SInt (SInt), SDouble (SDouble))
import Mascheya.Core.Result (Result, parseError, ParseError (Invalid, Eof))
import qualified Mascheya.Core.Lexemes as Lexemes

type Parser a = Parse (a, String)
type Parse a = String -> Result a

parse :: Parser a -> String -> Result a
parse parser = (>>= handle) . parser
  where
    handle (val, "") = return val
    handle (_, rest) = parseError $ Invalid "characters" rest

parseInt :: Parse SInt
parseInt = (SInt . read <$>) . parse int 

parseDouble :: Parse SDouble
parseDouble = (SDouble . read <$>) . parse double

int :: Parser String
int source = advance source >>= \(c, rest) ->
  if c == '-' 
  then (\(n, rest2) -> (c : n, rest2)) <$> nat rest 
  else nat source

double :: Parser String
double source = int source >>= \intResult@(whole, rest1) ->
  case advance rest1 of
    Left _ -> return intResult
    Right (c, rest2) -> 
      if c == Lexemes.dot 
      then (\(fractional, rest3) -> (whole ++ (c : fractional), rest3)) <$> nat rest2
      else return intResult

nat :: Parser String
nat = repeat digit

digit :: Parser Char
digit source = peek source >>= \c -> 
  if Char.isDigit c then advance source else parseError $ Invalid "digit" source

repeat :: Parser Char -> Parser String
repeat parser source = parser source >>= \(c, cs) -> case repeat parser cs of
  Left _ -> return (c : [], cs)
  Right (cs', css) -> return (c : cs', css)  

peek :: Parse Char
peek "" = parseError Eof
peek (c : _) = return c

advance :: Parser Char
advance [] = parseError Eof
advance (s : ss) = return (s, ss)