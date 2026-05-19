module Mascheya.Core.Ast.Core where

data Expr = Var Var | Lambda { param :: Var, body :: Expr } 
    | Const Const | Application { callable :: Expr, arg :: Expr } 
    deriving Show

data Var = VarData String deriving (Show, Eq)

data Const = Int Int | Char Char deriving Show

data BuiltinFunction = Arithmetic | Logical Logical 
    | Boolean Boolean | If Bool Expr Expr | ListFunction ListFunction
    deriving Show

data Arithmetic = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or | Not deriving Show
data Boolean = CTrue | CFalse deriving Show
data ListFunction = Cons Expr Expr | Head Expr | Tail Expr | Nil deriving Show

{- | Checks if the given variable occurs free in the expression. 
    Note that we are not using the phrase 'is free' because it gives the impression
    that the variable is not bound, which is misleading because a variable can both 
    occur free and occur bound in the same expression. -}
occursFree :: Var -> Expr -> Bool
occursFree x (Var y) = x == y
occursFree _ (Const _) = False
occursFree x (Application f e) = occursFree x f || occursFree x e
occursFree x (Lambda y e) = 
    -- if x and y are the same, then x is overshadowed and therefore not free
    x /= y && occursFree x e

{- | Checks if the given variable occurs bound in the expression. 
    Note that we are not using the phrase 'is bound' because it gives the impression
    that the variable is not free, which is misleading because a variable can both 
    occur free and occur bound in the same expression. -}
occursBound :: Var -> Expr -> Bool
occursBound _ (Var _) = False
occursBound _ (Const _) = False
occursBound x (Application f e) = occursBound x f || occursBound x e
occursBound x (Lambda y e) = occursBoundOuter || occursBoundInner
    where occursBoundOuter = x == y && occursFree x e
          occursBoundInner = occursBound x e
