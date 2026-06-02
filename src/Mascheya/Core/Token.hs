module Mascheya.Core.Token where
import Mascheya.Core.Display (Display (display))

data Token = Token {
  tokenType :: TokenType,
  lexeme :: String,
  line :: Int
} deriving (Show, Eq)

data TokenType = Identifier | Literal Literal | Eof deriving (Eq, Show)

data Literal = Int Int | Str String deriving (Eq, Show)

fromLine :: Int -> Token
fromLine = Token Eof ""

instance Display Token where
  display = lexeme

mkIdentifier :: String -> Int -> Token
mkIdentifier lexeme' line' = Token { tokenType = Identifier, lexeme = lexeme', line = line'}