module Mascheya.Core.Ast.Core where
import Mascheya.Core.Result (Loc)

data Expr = VarExpr CVar | LambdaExpr Lambda | ConstExpr Const 
  | AppExpr App | BuiltinFuncExpr BuiltinFunc Int
  deriving Show

data CVar = CVar { varName :: String, varLoc :: Loc } deriving (Show, Eq)

data Const = NumConst Numeric | CharConst CChar | BoolConst CBool deriving Show

data Numeric = CInt Int | CFloat Float | CDouble Double deriving Show

data CChar = CChar Char deriving Show

data CBool = CTrue | CFalse deriving Show

data Lambda = Lambda { param :: CVar, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr, source :: String, appLoc :: Loc } deriving Show

data BuiltinFunc = ArithFunc Arith | IfFunc If | ListFunc CList
  deriving Show

data Arith = Arith ArithKind Expr Expr deriving Show

data ArithKind = Plus | Minus | Times | Divide | Modulo deriving Show
data CList = Cons Expr Expr | Nil deriving Show
data If = If Expr Expr Expr deriving Show

newVar :: String -> Loc -> Expr
newVar name = VarExpr . CVar name

newLambda :: CVar -> Expr -> Expr
newLambda param' = LambdaExpr . Lambda param'

newInt :: Int -> Expr
newInt = newNumeric CInt 

newFloat :: Float -> Expr
newFloat = newNumeric CFloat

newDouble :: Double -> Expr
newDouble = newNumeric CDouble

newNumeric :: (a -> Numeric) -> a -> Expr
newNumeric f = ConstExpr . NumConst . f