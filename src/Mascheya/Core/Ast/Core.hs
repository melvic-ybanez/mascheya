module Mascheya.Core.Ast.Core where

data Expr = Var Var | Lambda { param :: Var, body :: Expr } 
    | Constant Constant | Application { callable :: Expr, arg :: Expr } 
    deriving Show

data Var = VarData String deriving (Show, Eq)

data Constant = Int Int | Char Char deriving Show

data BuiltinFunction = Arithmetic | Logical Logical 
    | Boolean Boolean | If Bool Expr Expr | ListFunction ListFunction
    deriving Show

data Arithmetic = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or | Not deriving Show
data Boolean = CTrue | CFalse deriving Show
data ListFunction = Cons Expr Expr | Head Expr | Tail Expr | Nil deriving Show

occursFree :: Var -> Expr -> Bool
occursFree x (Var y) = x == y
occursFree _ (Constant _) = False
occursFree x (Application f e) = occursFree x f || occursFree x e
occursFree x (Lambda y e) = 
    -- if x and y are the same, then x is overshadowed and therefore not free
    x /= y && occursFree x e

occursBound :: Var -> Expr -> Bool
occursBound _ (Var _) = False
occursBound _ (Constant _) = False
occursBound x (Application f e) = occursBound x f || occursBound x e
occursBound x (Lambda y e) = occursBoundOuter || occursBoundInner
    where occursBoundOuter = x == y && occursFree x e
          occursBoundInner = occursBound x e
