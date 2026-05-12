module Mascheya.Core.Token where

data Token = Token {
    tokenType :: TokenType,
    lexeme :: String,
    line :: Int
}

data TokenType = Literals Literals

data Literals = Number Double
