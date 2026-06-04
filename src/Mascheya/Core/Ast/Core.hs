module Mascheya.Core.Ast.Core where

import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display(display))
import Mascheya.Core.Token (Token)

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

newVar :: Token -> Expr
newVar = VarExpr . Var

newLambda :: Var -> Expr -> Expr
newLambda param' = LambdaExpr . Lambda param'

newApp :: Expr -> Expr -> Expr
newApp callable' = AppExpr . App callable'

newInt :: Int -> Expr
newInt = newNumeric CInt 

newFloat :: Float -> Expr
newFloat = newNumeric CFloat

newDouble :: Double -> Expr
newDouble = newNumeric CDouble

newNumeric :: (a -> Numeric) -> a -> Expr
newNumeric f = ConstExpr . NumConst . f

instance Display Expr where
  display (VarExpr var) = display var
  display (LambdaExpr (Lambda param' body')) = display Lexemes.lambdaSymbol ++ display param' 
    ++ display Lexemes.space ++ Lexemes.rightArrow 
    ++ display Lexemes.space ++ display body'
  display (ConstExpr constExpr) = display' constExpr
    where 
      display' (NumConst num) = case num of
        CInt int -> display int
        CFloat float -> display float
        CDouble double -> display double
      display' (CharConst (CChar char')) = display char'
  display (AppExpr (App func arg')) = display func ++ " " ++ display arg'
  display (BuiltinFuncExpr builtin _) = display' builtin
    where 
      display' (ArithFunc (Arith op _ _)) = display $ case op of
        Plus -> Lexemes.plus
        Minus -> Lexemes.minus
        Times -> Lexemes.times
        Divide -> Lexemes.divide
      display' (IfFunc (If cond ifTrue ifFalse)) = Lexemes.ifLexeme 
        ++ display Lexemes.space ++ display cond 
        ++ display Lexemes.space ++ display ifTrue 
        ++ display Lexemes.space ++ display ifFalse
      display' (ListFunc clist) = display clist
              
instance Display Var where
  display (Var token) = display token

instance Display CList where
  display Nil = display Lexemes.openSquareBracket ++ display Lexemes.closeSquareBracket
  display (Cons h t) = display h ++ display Lexemes.space 
    ++ Lexemes.cons ++ display Lexemes.space ++ display t