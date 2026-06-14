module Mascheya.Core.Ast.Source where

data Expr = LiteralExpr Literal deriving Show

data Literal = NumLit SNum | CharLit SChar | BoolLit SBool deriving Show

data SNum = IntLit SInt | FloatLit SFloat | DoubleLit SDouble deriving Show
data SBool = STrue | SFalse deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show