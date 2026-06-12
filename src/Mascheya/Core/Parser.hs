{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Mascheya.Core.Parser where

import Mascheya.Core.Result (Result, parseError, ParseError (Eof, Expected, Invalid))
import qualified Mascheya.Core.Result as Result
import Prelude hiding (repeat, fail)
import Data.Char 
import Control.Applicative (Alternative (empty))
import GHC.Base (Alternative((<|>)))
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Ast.Source

data State = State { source :: String, line :: Int }
newtype Parser a = Parser { run :: State -> Result (a, State) }

newState :: String -> State
newState = flip State 1

parse :: Parser a -> String -> Result a
parse (Parser run') = (>>= handle) . run' . newState 
  where
    handle (val, (State [] _)) = Result.succeed val
    handle (_, (State rest line')) = parseError (Invalid "characters" rest) line'

sLiteral :: Parser Literal
sLiteral = NumLit <$> sNum <|> CharLit <$> sChar

sNum :: Parser SNum
sNum = floatLit <|> doubleLit <|> intLit
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

sChar :: Parser SChar
sChar = (\((_, content), _) -> SChar content) <$> char Lexemes.singleQuote 
  <&> (escaped <|> unescaped) <&> char Lexemes.singleQuote

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

unescaped :: Parser Char
unescaped = lower <|> upper <|> digit <|> symbol <|> special <|> space <|> ascii

escaped :: Parser Char
escaped = withLine (codePoint <|> (str [Lexemes.escapePrefix] <&> (charesc <|> ascii))) >>= unescape
  where 
    unescape ((prefix, ch), line') = case readLitChar $ prefix ++ [ch] of
      [(result, _)] -> return result
      _ -> fail (Expected "escapeable character") line'
    codePoint = digit' <|> octal' <|> hex'
    digit' = str [Lexemes.escapePrefix] <&> digit
    octal' = str Lexemes.octalPrefix <&> octal
    hex' = str Lexemes.hexPrefix <&> hex

repeat :: Parser a -> Parser [a]
repeat pa = pa >>= \a -> Parser {
  run = \state -> case run (repeat pa) state of
    Left _ -> Result.succeed ([a], state)
    Right (as, rest) -> Result.succeed (a : as, rest)
}

str :: String -> Parser String
str [] = return []
str (x : xs) = (\(c, cs) -> c : cs) <$> char x <&> str xs

fail :: ParseError -> Int -> Parser a
fail err = Parser . const . parseError err

char :: Char -> Parser Char
char c = charExpect c [c]

charExpect :: Char -> String -> Parser Char
charExpect c = satisfyExpect (\a -> a == c) 

digit :: Parser Char
digit = satisfyExpect isDigit "digit"

lower :: Parser Char
lower = satisfyExpect isLower "lower-case character"

upper :: Parser Char
upper = satisfyExpect isUpper "uper-case character"

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

space :: Parser Char
space = satisfyExpect isSpace "space character"

ascii :: Parser Char
ascii = satisfyExpect isAscii "control character"

symbol :: Parser Char
symbol = satisfyExpect isSymbol "symbolic character"

special :: Parser Char
special = satisfyExpect (`elem` "(),;[]`{}") "special character"

hex :: Parser Char
hex = satisfyExpect isHexDigit "hexadecimal"

octal :: Parser Char
octal = satisfyExpect isOctDigit "octal"

charesc :: Parser Char
charesc = satisfyExpect (`elem` "'\"\\nrtvb") "escapeable character"

word :: Parser String
word = nonEmpty <|> return ""
  where 
    nonEmpty = (\(x, xs) -> x : xs) <$> letter <&> word

item :: Parser Char
item = Parser $ \(State inp line') -> case inp of
  [] -> parseError Eof line'
  (x : xs) -> Result.succeed (x, State xs (if x == '\n' || x == '\r' then line' + 1 else line'))

satisfyExpect :: (Char -> Bool) -> String -> Parser Char
satisfyExpect p e = satisfy p $ Expected e

satisfy :: (Char -> Bool) -> ParseError -> Parser Char
satisfy p e = withLine item >>= \(x, l) -> if p x then return x else fail e l

withLine :: Parser a -> Parser (a, Int)
withLine p = p >>= \a -> Parser {
  run = \inp@(State _ line') -> Result.succeed ((a, line'), inp)
}

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
  empty = fail Eof 0
  
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser runA) (Parser runB) = Parser {
    run = \inp -> case runA inp of
      Left _ -> runB inp
      right -> right 
  }
