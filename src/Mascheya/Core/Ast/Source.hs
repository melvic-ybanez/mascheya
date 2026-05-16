module Mascheya.Core.Ast.Source where

data Expr = Literal Literal deriving Show

data Literal = Int Int deriving Show