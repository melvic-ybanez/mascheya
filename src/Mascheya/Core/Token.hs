module Mascheya.Core.Token where

data Token = Token {
    tokenType :: TokenType,
    lexeme :: String,
    line :: Int
}

data TokenType = Literal Literal | Eof deriving Eq

data Literal = Int Int | Str String deriving Eq
