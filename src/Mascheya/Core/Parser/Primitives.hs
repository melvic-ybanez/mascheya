module Mascheya.Core.Parser.Primitives where

import Prelude hiding (pred, fail, repeat)

import Mascheya.Core.Parser.Core 
import Mascheya.Core.Result (ParseError (Expected), parseError, Loc (Loc))
import qualified Mascheya.Core.Lexemes as Lexemes
import Control.Applicative ((<|>))
import Data.Char
import Data.Either (isRight)
import Data.List (find)

-- | Parser for the function and/or variable identifier.
-- It must start with either a lower case letter or an underscore. 
-- The rest of the characters can be letters (lower or upper), digits,
-- underscores and single quotes.
functionId :: Parser (String, Loc)
functionId = combine <$> (track $ validHead <&> opt validTail)
  where
    validHead = lower <|> underscore
    validTail = repeat $ letter <|> digit <|> underscore <|> singleQuote

    combine ((h, Nothing), l) = ([h], l)
    combine ((h, Just t), l) = (h : t, l)

symbolicFunc :: Parser (String, Loc)
symbolicFunc = track $ repeat $ satisfyExpect pred "function symbol character"
  where
    -- the hardcoded charcacters are based on the valid symbols in Scala and Haskell,
    -- ordered in ascending order of precedence 
    pred c = c `elem` "|^&<>=!:+-*/%#$?@\\~" || isSymbol c

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
    unescape ((prefix, val), Loc line') = case readLitChar $ prefix ++ val of
      [(result, _)] -> return result
      _ -> fail (Expected "escapeable character") $ Loc line'
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
special = chooseCharExpect "(),;[]`{}" "special character"

hex :: Parser Char
hex = satisfyExpect isHexDigit "hexadecimal"

octal :: Parser Char
octal = satisfyExpect isOctDigit "octal"

singleEsc :: Parser Char
singleEsc = chooseCharExpect "'\"\\nrtvb" "single escapeable character"

asciiEsc :: Parser String
asciiEsc = Parser $ \state@(State _ line') -> maybe
  (parseError (Expected "ascii control code") $ Loc line') id 
  $ find isRight 
  $ map (flip parseCode state) controlCodes
  where
    parseCode code = run $ str code
    controlCodes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", 
      "HT", "LF", "VT", "FF", "CR", "SO","SI", "DLE", "DC1", "DC2", "DC3", "DC4", 
      "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "DEL"]

inParens :: Parser a -> Parser a
inParens p = bracket Lexemes.leftParen p Lexemes.rightParen

caretControl :: Parser Char
caretControl = upper <|> chooseCharExpect "[]\\^_" "caret control"

underscore :: Parser Char
underscore = char Lexemes.underscore

singleQuote :: Parser Char
singleQuote = char Lexemes.singleQuote