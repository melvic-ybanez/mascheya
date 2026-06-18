module Mascheya.Core.Ast.Core where
import Mascheya.Core.Result (Loc)

data CExpr = CVarExpr CVar | CLambdaExpr CLambda | CConstExpr CConst 
  | CAppExpr CApp | CBuiltinFuncExpr CBuiltinFunc
  deriving Show

data CVar = CVar { varName :: String, varLoc :: Loc } deriving (Show, Eq)

data CConst = CNumConst CNumeric | CCharConst CChar | CBoolConst CBool deriving Show

data CNumeric = CInt Int | CFloat Float | CDouble Double deriving Show

data CChar = CChar Char deriving Show

data CBool = CTrue | CFalse deriving Show

data CLambda = CLambda { param :: CVar, body :: CExpr } deriving Show
data CApp = CApp { callable :: CExpr, arg :: CExpr, source :: String, appLoc :: Loc } deriving Show

data CBuiltinFunc = CArithFunc CArith | CIfFunc CIf | CListFunc CList
  deriving Show

data CArith = CArith CArithKind CExpr CExpr deriving Show

data CArithKind = CPlus | CMinus | CTimes | CDivide | CModulo deriving Show
data CList = CCons CExpr CExpr | CNil deriving Show
data CIf = CIf CExpr CExpr CExpr deriving Show

newVar :: String -> Loc -> CExpr
newVar name = CVarExpr . CVar name

newLambda :: CVar -> CExpr -> CExpr
newLambda param' = CLambdaExpr . CLambda param'

newInt :: Int -> CExpr
newInt = newNumeric CInt 

newFloat :: Float -> CExpr
newFloat = newNumeric CFloat

newDouble :: Double -> CExpr
newDouble = newNumeric CDouble

newNumeric :: (a -> CNumeric) -> a -> CExpr
newNumeric f = CConstExpr . CNumConst . f