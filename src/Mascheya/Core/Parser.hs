module Mascheya.Core.Parser where

import qualified Data.Char as Char
import Prelude hiding (repeat)
import Mascheya.Core.Ast.Source (Literal(SInt), Expr (Literal))
import Mascheya.Core.Result (Result, parseError, ParseError (Invalid, Eof))

type Parser a = String -> Result (a, String)

parse :: Parser a -> String -> Result a
parse parser = (>>= handle) . parser
  where
    handle (val, "") = return val
    handle (_, rest) = parseError $ Invalid "characters" rest

parseInt :: String -> Result Expr
parseInt = (Literal . SInt . read <$>) . parse int 

int :: Parser String
int source = peek source >>= \c ->
  if c == '-' 
  then (\(cs, css) -> (c : cs, css)) <$> (advance source >>= \(_, rest) -> nat rest) 
  else nat source

nat :: Parser String
nat = repeat digit

digit :: Parser Char
digit source = peek source >>= \c -> 
  if Char.isDigit c then advance source else parseError $ Invalid "digit" source

repeat :: Parser Char -> Parser String
repeat parser source = parser source >>= \(c, cs) -> case repeat parser cs of
  Left _ -> return (c : [], cs)
  Right (cs', css) -> return (c : cs', css)  

peek :: String -> Result Char
peek "" = parseError Eof
peek (c : _) = return c

advance :: Parser Char
advance [] = parseError Eof
advance (s : ss) = return (s, ss)
