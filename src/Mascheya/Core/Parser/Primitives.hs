module Mascheya.Core.Parser.Primitives where

import Prelude hiding (pred, fail, repeat)

import Mascheya.Core.Parser.Core 
import Mascheya.Core.Result (ParseError (Expected, Eof), parseError)
import qualified Mascheya.Core.Lexemes as Lexemes
import Control.Applicative ((<|>))
import Data.Char
import qualified Mascheya.Core.Result as Result
import Data.Either (isRight)
import Data.List (find)

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
escaped = track escapedPair >>= unescape
  where 
    escapedPair = codePoint <|> 
      (str [Lexemes.escapePrefix] <&> (caretControl' <|> singleEsc' <|> asciiEsc))
    unescape ((prefix, val), line') = case readLitChar $ prefix ++ val of
      [(result, _)] -> return result
      _ -> fail (Expected "escapeable character") line'
    codePoint = digit' <|> octal' <|> hex'
    digit' = str [Lexemes.escapePrefix] <&> repeat digit
    octal' = str Lexemes.octalPrefix <&> repeat octal
    hex' = str Lexemes.hexPrefix <&> repeat hex
    singleEsc' = (: []) <$> singleEsc
    caretControl' = (\(c, d) -> [c, d]) <$> char '^' <&> caretControl

true :: Parser String
true = str Lexemes.true

false :: Parser String
false = str Lexemes.false

repeat :: Parser a -> Parser [a]
repeat pa = pa >>= \a -> Parser {
  run = \state -> case run (repeat pa) state of
    Left _ -> Result.succeed ([a], state)
    Right (as, rest) -> Result.succeed (a : as, rest)
}

str :: String -> Parser String
str src = strExpect src src

strExpect :: String -> String -> Parser String
strExpect [] _ = return []
strExpect (x : xs) err = (\(c, cs) -> c : cs) <$> charExpect x err <&> strExpect xs err

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

singleEsc :: Parser Char
singleEsc = satisfyExpect (`elem` "'\"\\nrtvb") "single escapeable character"

asciiEsc :: Parser String
asciiEsc = Parser $ \state@(State _ line') -> maybe
  (parseError (Expected "ascii control code") line') id 
  $ find isRight 
  $ map (flip parseCode state) controlCodes
  where
    parseCode code = run $ str code
    controlCodes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", 
      "HT", "LF", "VT", "FF", "CR", "SO","SI", "DLE", "DC1", "DC2", "DC3", "DC4", 
      "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "DEL"]

caretControl :: Parser Char
caretControl = upper <|> satisfyExpect (`elem` "[]\\^_") "caret control"

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
satisfy p e = track item >>= \(x, l) -> if p x then return x else fail e l