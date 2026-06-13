module Mascheya.Core.Parser.Primitives where

import Prelude hiding (fail, repeat)

import Mascheya.Core.Parser.Core 
import Mascheya.Core.Result (ParseError (Expected, Eof), parseError)
import qualified Mascheya.Core.Lexemes as Lexemes
import Control.Applicative ((<|>))
import Data.Char
import qualified Mascheya.Core.Result as Result

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