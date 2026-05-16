module Mascheya.Core.Token where

data Token = Token {
    tokenType :: TokenType,
    lexeme :: String,
    line :: Int
} deriving Show

data TokenType = Literal Literal | Eof deriving (Eq, Show)

data Literal = Int Int | Str String deriving (Eq, Show)

fromLine :: Int -> Token
fromLine = Token Eof ""