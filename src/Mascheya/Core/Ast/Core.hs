module Mascheya.Core.Ast.Core where

data Expr = Var Var | Abstraction { param :: Var, body :: Expr } 
    | Constant Constant | Application { functionName :: String, arg :: Expr } 
    deriving Show

data Var = VarData { name :: String, rhs :: Expr } deriving Show

data Constant = Int Int | Char Char deriving Show

data BuiltinFunction = Arithmetic | Logical Logical 
    | Boolean Boolean | If Bool Expr Expr | ListFunction ListFunction
    deriving Show

data Arithmetic = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or | Not deriving Show
data Boolean = True | False deriving Show
data ListFunction = Cons Expr Expr | Head Expr | Tail Expr | Nil deriving Show