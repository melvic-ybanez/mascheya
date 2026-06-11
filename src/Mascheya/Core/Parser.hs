{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Mascheya.Core.Parser where

import Mascheya.Core.Result (Result, parseError, ParseError (Eof, Expected, Invalid))
import qualified Mascheya.Core.Result as Result
import Prelude hiding (repeat, fail)
import Data.Char (isDigit, isLower, isUpper)
import Control.Applicative (Alternative (empty))
import GHC.Base (Alternative((<|>)))
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Ast.Source

data State = State { source :: String, line :: Int }
newtype Parser a = Parser { run :: State -> Result (a, State) }

newState :: String -> State
newState = flip State 0

parse :: Parser a -> String -> Result a
parse (Parser run') = (>>= handle) . run' . newState 
  where
    handle (val, (State [] _)) = Result.succeed val
    handle (_, (State rest _)) = parseError $ Invalid "characters" rest

sLit :: Parser Literal
sLit = floatLit <|> doubleLit <|> intLit
  where
    intLit = IntLit <$> sInt
    doubleLit = DoubleLit <$> sDouble
    floatLit = FloatLit <$> sFloat

sInt :: Parser SInt
sInt = parseMap SInt int

sDouble :: Parser SDouble
sDouble = parseMap SDouble double

sFloat :: Parser SFloat
sFloat = parseMap SFloat float

parseMap :: Read a => (a -> b) -> Parser String -> Parser b
parseMap f = ((f . read) <$> ) 

int :: Parser String
int = repeat digit

double :: Parser String
double = combine <$> int <&> char Lexemes.dot <&> int
  where 
    combine ((whole, dot), frac) = whole ++ [dot] ++ frac

float :: Parser String
float = fst <$> double <&> char Lexemes.floatSuffix

repeat :: Parser a -> Parser [a]
repeat pa = pa >>= \a -> Parser {
  run = \state -> case run (repeat pa) state of
    Left _ -> Result.succeed ([a], state)
    Right (as, rest) -> Result.succeed (a : as, rest)
}

fail :: ParseError -> Parser a
fail = Parser . const . parseError

char :: Char -> Parser Char
char = flip satExpect "character" . (==)

digit :: Parser Char
digit = satExpect isDigit "digit"

lower :: Parser Char
lower = satExpect isLower "lower-case character"

upper :: Parser Char
upper = satExpect isUpper "uper-case character"

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

word :: Parser String
word = nonEmpty <|> return ""
  where 
    nonEmpty = (\(x, xs) -> x : xs) <$> letter <&> word

item :: Parser Char
item = Parser $ \(State inp line') -> case inp of
  [] -> parseError Eof
  (x : xs) -> Result.succeed (x, State xs (if x == '\n' then line' + 1 else line'))

satExpect :: (Char -> Bool) -> String -> Parser Char
satExpect p e = sat p $ Expected e

sat :: (Char -> Bool) -> ParseError -> Parser Char
sat p e = item >>= \x -> if p x then pure x else fail e

(<&>) :: Parser a -> Parser b -> Parser (a, b)
(<&>) p q = do
  a <- p
  b <- q
  return (a, b)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = p >>= \res -> return $ f res

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ Result.succeed . (v, )

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pab pa = do
    f <- pab
    a <- pa
    return $ f a

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser pa) f = Parser {
    run = \inp -> pa inp >>= \(val, rest) -> run (f val) rest
  }

instance Alternative Parser where  
  empty :: Parser a
  empty = fail Eof
  
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser runA) (Parser runB) = Parser {
    run = \inp -> case runA inp of
      Left _ -> runB inp
      right -> right 
  }
