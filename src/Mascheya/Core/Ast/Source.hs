module Mascheya.Core.Ast.Source where

data Expr = Literal Literal deriving Show

data Literal = IntLit SInt | FloatLit SFloat | DoubleLit SDouble | SChar Char deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show