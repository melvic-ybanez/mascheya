module Mascheya.Core.Token where
import Mascheya.Core.Display (Display (display))

data Token = Token {
    tokenType :: TokenType,
    lexeme :: String,
    line :: Int
} deriving (Show, Eq)

data TokenType = Literal Literal | Eof deriving (Eq, Show)

data Literal = Int Int | Str String deriving (Eq, Show)

fromLine :: Int -> Token
fromLine = Token Eof ""

instance Display Token where
    display = lexeme