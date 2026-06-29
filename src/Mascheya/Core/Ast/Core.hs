module Mascheya.Core.Ast.Core where
  
import Mascheya.Core.Result (Loc, dummyLoc)
import Data.List.NonEmpty (NonEmpty)

data CProg = CDefProg CDef | CExprProg CExpr deriving Show

data CExpr = CVarExpr CVar | CLambdaExpr CLambda | CConstExpr CConst 
  | CAppExpr CApp | CBuiltinFuncExpr CBuiltinFunc | CLetExpr CLet 
  deriving Show

data CVar = CVar { varName :: String, varLoc :: Loc } deriving (Show, Eq)

data CConst = CNumConst CNumeric | CCharConst CChar | CBoolConst CBool deriving Show

data CNumeric = CInt Int | CFloat Float | CDouble Double deriving Show

data CChar = CChar Char deriving Show

data CBool = CTrue | CFalse deriving Show

data CLambda = CLambda { param :: CVar, body :: CExpr } deriving Show
data CApp = CApp { 
  callable :: CExpr, 
  arg :: CExpr, 
  appSource :: String, 
  appLoc :: Loc 
} deriving Show

data CBuiltinFunc = CInfixFunc CInfix | CIfFunc CIf | CListFunc CList deriving Show

data CInfix = CInfix CExpr CInfixOp CExpr deriving Show

data CArith = CPlus | CMinus | CTimes | CDivide | CModulo deriving Show
data CList = CCons CExpr CExpr | CNil deriving Show
data CIf = CIf CExpr CExpr CExpr deriving Show

data CInfixOp = CArithOp CArith | CCompOp CComp deriving Show
data CComp = CEqEq | CNotEq | CLt | CLte | CGt | CGte
  deriving Show

data CLet = CLet (NonEmpty CDef) CExpr deriving Show

data CDef = CDef { 
  lhs :: CExpr, 
  rhs :: CExpr, 
  defSource :: String, 
  defLoc :: Loc 
} deriving Show

newVar :: String -> Loc -> CExpr
newVar name = CVarExpr . CVar name

newDummyVar :: String -> CVar
newDummyVar = flip CVar dummyLoc

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

newBool :: Bool -> CExpr
newBool b = CConstExpr $ CBoolConst $ case b of 
  True -> CTrue
  False -> CFalse