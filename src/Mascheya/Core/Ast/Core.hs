{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mascheya.Core.Ast.Core where
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display(display))
import Mascheya.Core.Token

data Expr = VarExpr Var | LambdaExpr Lambda | ConstExpr Const 
    | AppExpr App | BuiltinFuncExpr BuiltinFunc Int
    deriving Show

newtype Var = Var Token deriving (Show, Eq)

data Const = NumConst Numeric | CChar Char | CBool Bool deriving Show
data Numeric = CInt Int | CFloat Float | CDouble Double deriving Show

data Lambda = Lambda { param :: Var, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr } deriving Show

data BuiltinFunc = ArithFunc ArithKind Numeric Numeric | LogicalFunc Logical | IfFunc If | ListFunc CList
    deriving Show

data ArithKind = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or deriving Show
data CList = Cons Expr Expr | Nil deriving Show
data If = If Bool Expr Expr deriving Show

mkVar :: Token -> Expr
mkVar = VarExpr . Var

mkArith :: ArithKind -> Numeric -> Numeric -> Int -> Expr
mkArith arith a = BuiltinFuncExpr . ArithFunc arith a

mkLambda :: Var -> Expr -> Expr
mkLambda param' = LambdaExpr . Lambda param'

mkApp :: Expr -> Expr -> Expr
mkApp callable' = AppExpr . App callable'

mkInt :: Int -> Expr
mkInt = mkNumeric CInt 

mkFloat :: Float -> Expr
mkFloat = mkNumeric CFloat

mkDouble :: Double -> Expr
mkDouble = mkNumeric CDouble

mkNumeric :: (a -> Numeric) -> a -> Expr
mkNumeric f = ConstExpr . NumConst . f

instance Display Expr where
    display (VarExpr var) = display var
    display (LambdaExpr (Lambda param' body')) = Lexemes.lambdaSymbol ++ display param' ++ " " 
        ++ Lexemes.rightArrow ++ " " ++ display body'
    display (ConstExpr constExpr) = display' constExpr
        where display' (NumConst (CInt int)) = display int
              display' (NumConst (CFloat float)) = display float
              display' (NumConst (CDouble double)) = display double
              display' (CChar char') = display char'
              display' (CBool bool) = display bool
    display (AppExpr (App func arg')) = display func ++ " " ++ display arg'
    display (BuiltinFuncExpr builtin _) = display' builtin
        where display' (ArithFunc Plus _ _) = Lexemes.plus
              display' (ArithFunc Minus _ _) = Lexemes.minus
              display' (ArithFunc Times _ _) = Lexemes.times
              display' (ArithFunc Divide _ _) = Lexemes.divide
              display' (LogicalFunc And) = Lexemes.and
              display' (LogicalFunc Or) = Lexemes.or
              display' (IfFunc (If cond ifTrue ifFalse)) = Lexemes.ifLexeme ++ " " ++ display cond 
                ++ " " ++ display ifTrue ++ " " ++ display ifFalse
              display' (ListFunc Nil) = Lexemes.openSquareBracket ++ Lexemes.closeSquareBracket
              display' (ListFunc (Cons head' tail')) = display head' ++ " " ++ Lexemes.cons ++ " " ++ display tail'
              
instance Display Var where
    display (Var token) = display token