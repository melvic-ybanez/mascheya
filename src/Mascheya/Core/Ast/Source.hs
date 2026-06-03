module Mascheya.Core.Ast.Source where

data Expr = Literal Literal deriving Show

data Literal = SInt Int | SFloat Float | SDouble Double | SChar Char deriving Show