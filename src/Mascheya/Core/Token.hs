module Mascheya.Core.Token where
import Mascheya.Core.Display (Display (display))

data Token = Token {
  tokenType :: TokenType,
  lexeme :: String,
  line :: Int
} deriving (Show, Eq)

data TokenType = Identifier | LiteralType Literal 
  | Equals | DoubleEquals 
  | LeftParen | RightParen 
  | LambdaSymbol | Eof deriving (Eq, Show)

data Literal = TInt Int | TFloat Float | TDouble Double | TChar Char deriving (Eq, Show)

fromLine :: Int -> Token
fromLine = Token Eof ""

instance Display Token where
  display = lexeme

mkIdentifier :: String -> Int -> Token
mkIdentifier lexeme' line' = Token { tokenType = Identifier, lexeme = lexeme', line = line'}