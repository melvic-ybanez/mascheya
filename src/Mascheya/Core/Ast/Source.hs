module Mascheya.Core.Ast.Source where

data Expr = LiteralExpr Literal deriving Show

data Literal = NumLit SNum | CharLit SChar deriving Show

data SNum = IntLit SInt | FloatLit SFloat | DoubleLit SDouble deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show