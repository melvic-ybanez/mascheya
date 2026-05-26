{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mascheya.Core.Ast.Core where
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display(display))
import Mascheya.Core.Token

data Expr = VarExpr Var | LambdaExpr Lambda | ConstExpr Const 
    | AppExpr App | BuiltinFuncExpr BuiltinFunc
    deriving Show

newtype Var = Var Token deriving (Show, Eq)
data Const = CInt Int | CChar Char | CFloat Float | CDouble Double | CBool Bool deriving Show
data Lambda = Lambda { param :: Var, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr } deriving Show

data BuiltinFunc = ArithFunc Arith | LogicalFunc Logical | IfFunc If | ListFunc CList
    deriving Show

data Arith = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or deriving Show
data CList = Cons Expr Expr | Head Expr | Tail Expr | Nil deriving Show
data If = If Bool Expr Expr deriving Show

mkVar :: Token -> Expr
mkVar = VarExpr . Var

mkInt :: Int -> Expr
mkInt = ConstExpr . CInt

mkArith :: Arith -> Expr
mkArith = BuiltinFuncExpr . ArithFunc

mkLambda :: Var -> Expr -> Expr
mkLambda param' = LambdaExpr . Lambda param'

mkApp :: Expr -> Expr -> Expr
mkApp callable' = AppExpr . App callable'

instance Display Expr where
    display (VarExpr var) = display var
    display (LambdaExpr (Lambda param' body')) = Lexemes.lambdaSymbol ++ display param' ++ " " 
        ++ Lexemes.rightArrow ++ " " ++ display body'
    display (ConstExpr constExpr) = display' constExpr
        where display' (CInt int) = display int
              display' (CChar char') = display char'
              display' (CFloat float) = display float
              display' (CDouble double) = display double
              display' (CBool bool) = display bool
    display (AppExpr (App func arg')) = display func ++ " " ++ display arg'
    display (BuiltinFuncExpr builtin) = display' builtin
        where display' (ArithFunc Plus) = Lexemes.plus
              display' (ArithFunc Minus) = Lexemes.minus
              display' (ArithFunc Times) = Lexemes.times
              display' (ArithFunc Divide) = Lexemes.divide
              display' (LogicalFunc And) = Lexemes.and
              display' (LogicalFunc Or) = Lexemes.or
              display' (IfFunc (If cond ifTrue ifFalse)) = Lexemes.ifLexeme ++ " " ++ display cond 
                ++ " " ++ display ifTrue ++ " " ++ display ifFalse
              
instance Display Var where
    display (Var token) = display token