module Mascheya.Core.Ast.Core where

import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display(display))
import Mascheya.Core.Token

data Expr = VarExpr Var | LambdaExpr Lambda | ConstExpr Const 
    | AppExpr App | BuiltinFuncExpr BuiltinFunc Int
    deriving Show

newtype Var = Var Token deriving (Show, Eq)

data Const = NumConst Numeric | CharConst CChar deriving Show

data Numeric = CInt Int | CFloat Float | CDouble Double deriving Show

data CChar = CChar Char deriving Show

data Lambda = Lambda { param :: Var, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr } deriving Show

data BuiltinFunc = ArithFunc Arith | IfFunc If | ListFunc CList
    deriving Show

data Arith = Arith ArithKind Expr Expr deriving Show

data ArithKind = Plus | Minus | Times | Divide deriving Show
data CList = Cons Expr Expr | Nil deriving Show
data If = If Expr Expr Expr deriving Show

mkVar :: Token -> Expr
mkVar = VarExpr . Var

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
              display' (CharConst (CChar char')) = display char'
    display (AppExpr (App func arg')) = display func ++ " " ++ display arg'
    display (BuiltinFuncExpr builtin _) = display' builtin
        where display' (ArithFunc (Arith Plus _ _)) = Lexemes.plus
              display' (ArithFunc (Arith Minus _ _)) = Lexemes.minus
              display' (ArithFunc (Arith Times _ _)) = Lexemes.times
              display' (ArithFunc (Arith Divide _ _)) = Lexemes.divide
              display' (IfFunc (If cond ifTrue ifFalse)) = Lexemes.ifLexeme ++ " " ++ display cond 
                ++ " " ++ display ifTrue ++ " " ++ display ifFalse
              display' (ListFunc clist) = display clist
              
instance Display Var where
    display (Var token) = display token

instance Display CList where
    display Nil = Lexemes.openSquareBracket ++ Lexemes.closeSquareBracket
    display (Cons h t) = display h ++ " " ++ Lexemes.cons ++ " " ++ display t