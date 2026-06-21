module Mascheya.Core.Parser.Primitives where

import Prelude hiding (pred, fail, repeat)

import Mascheya.Core.Parser.Core 
import Mascheya.Core.Result (ParseError (Expected), parseError, Loc (Loc))
import qualified Mascheya.Core.Lexemes as Lexemes
import Control.Applicative ((<|>))
import Data.Char
import Data.Either (isRight)
import Data.List (find)
import Data.List.NonEmpty (toList)

-- | Parser for the function and/or variable identifier.
-- It must start with either a lower case letter or an underscore. 
-- The rest of the characters can be letters (lower or upper), digits,
-- underscores and single quotes.
functionId :: Parser (String, Loc)
functionId = (\((h, t), l) -> (h : t, l)) <$> (track $ validHead <&> validTail)
  where
    validHead = lower <|> underscore
    validTail = repeat0 $ letter <|> digit <|> underscore <|> singleQuote

rawInt :: Parser String
rawInt = toRawInt <$> opt plusOrMinus <&> (toList <$> repeat digit)
  where
    toRawInt (Nothing, digits) = digits
    toRawInt (Just sign, digits) | sign == Lexemes.minus = sign : digits
      | otherwise = digits

rawDouble :: Parser String
rawDouble = combine <$> rawInt <&> matchChar Lexemes.dot <&> rawInt
  where 
    combine ((whole, dot), frac) = whole ++ [dot] ++ frac

rawFloat :: Parser String
rawFloat = fst <$> rawDouble <&> matchChar Lexemes.floatSuffix

unescaped :: Parser Char
unescaped = lower <|> upper <|> digit <|> symbol <|> special <|> space <|> ascii

escaped :: Parser Char
escaped = track escapedPair >>= unescape
  where 
    escapedPair = codePoint <|> 
      (matchStr [Lexemes.escapePrefix] <&> (caretControl' <|> singleEsc' <|> asciiEsc))
    unescape ((prefix, val), Loc line') = case readLitChar $ prefix ++ val of
      [(result, _)] -> return result
      _ -> fail (Expected "escapeable character") $ Loc line'
    codePoint = digit' <|> octal' <|> hex'
    digit' = matchStr [Lexemes.escapePrefix] <&> rawInt
    octal' = matchStr Lexemes.octalPrefix <&> (toList <$> repeat octal)
    hex' = matchStr Lexemes.hexPrefix <&> (toList <$> repeat hex)
    singleEsc' = (: []) <$> singleEsc
    caretControl' = (\(c, d) -> [c, d]) <$> matchChar '^' <&> caretControl

true :: Parser String
true = matchStr Lexemes.true

false :: Parser String
false = matchStr Lexemes.false

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
    parseCode code = run $ matchStr code
    controlCodes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", 
      "HT", "LF", "VT", "FF", "CR", "SO","SI", "DLE", "DC1", "DC2", "DC3", "DC4", 
      "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "DEL"]

inParens :: Parser a -> Parser a
inParens p = bracket Lexemes.leftParen p Lexemes.rightParen

caretControl :: Parser Char
caretControl = upper <|> chooseCharExpect "[]\\^_" "caret control"

underscore :: Parser Char
underscore = matchChar Lexemes.underscore

singleQuote :: Parser Char
singleQuote = matchChar Lexemes.singleQuote

plusOrMinus :: Parser Char
plusOrMinus = matchChar Lexemes.plus <|> matchChar Lexemes.minus

termOp :: Parser Char
termOp = matchChar Lexemes.times <|> matchChar Lexemes.divide <|> matchChar Lexemes.modulo

skipSpaces :: Parser ()
skipSpaces = (const ()) <$> repeat0 space

inSpaces :: Parser a -> Parser a
inSpaces p = (\((_, a), _) -> a) <$> skipSpaces <&> p <&> skipSpaces