module Mascheya.Core.Ast.Core where
  
import Mascheya.Core.Result (Loc, dummyLoc)
import Data.List.NonEmpty (NonEmpty)
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display (display), SSV (SSV), Str (Str), Tup ((:+:)))
import Data.List (intercalate)
import Text.Printf (printf)
import qualified Data.List.NonEmpty as NonEmpty

data CProg = CDefNelProg CDefNel | CExprProg CExpr deriving Show

data CExpr = CVarExpr CVar | CLambdaExpr CLambda | CConstExpr CConst 
  | CAppExpr CApp | CBuiltinFuncExpr CBuiltinFunc | CLetExpr CLet 
  | CConstructorExpr CConstructor | COrElseExpr COrElse | CProdExpr CProd | CBottomExpr
  deriving Show

data CVar = CVar { varName :: String, varLoc :: Loc } deriving (Show, Eq)

data CConst = CNumConst CNumeric | CCharConst CChar | CBoolConst CBool | CUnitConst deriving Show

data CNumeric = CInt Int | CFloat Float | CDouble Double deriving Show

data CChar = CChar Char deriving Show

data CBool = CTrue | CFalse deriving Show

data CLambda = CLambda { lambdaParam :: CPat, lambdaBody :: CExpr } deriving Show
data CApp = CApp { 
  appCallable :: CExpr, 
  appArg :: CExpr, 
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

data CLet = CLet CDefNel CExpr deriving Show

newtype CDefNel = CDefNel (NonEmpty CDef) deriving Show

data CDef = CDef { 
  defName :: CVar,
  defRhs :: CExpr, 
  defSource :: String
} deriving Show

data CProd = CProd String [CExpr] deriving Show

data CConstructor = CConstructor String Loc deriving Show

data CPat = CVarPat CVar | CConstPat CConst
  | CConstructorPat CConstructor [CPat] deriving Show

data COrElse = COrElse CExpr CExpr Loc deriving Show 

unit :: CExpr
unit = CProdExpr $ CProd Lexemes.unit []

newVar :: String -> Loc -> CExpr
newVar name = CVarExpr . CVar name

newDummyVar :: String -> CVar
newDummyVar = flip CVar dummyLoc

newLambda :: CPat -> CExpr -> CExpr
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

fromLambdaDetails :: [CPat] -> CExpr -> CExpr
fromLambdaDetails patterns = flip (foldr mkLambdaExpr) patterns
  where 
    mkLambdaExpr param' acc = CLambdaExpr $ CLambda param' acc 

instance Display CProg where
  display (CDefNelProg defNel) = display defNel
  display (CExprProg expr) = display expr

instance Display CExpr where
  display (CVarExpr var) = display var
  display (CLambdaExpr (CLambda param body)) = display 
    $ (Str $ Lexemes.lambdaSymbol : display param) 
    :+: Str Lexemes.rightArrow :+: body
  display (CConstExpr const') = display const'
  display (CAppExpr (CApp callable arg _ _)) = display $ SSV [callable, arg]
  display (CBuiltinFuncExpr builtin) = case builtin of
    CInfixFunc (CInfix left op right) -> display $ left :+: op :+: right
    CIfFunc (CIf cond ifTrue ifFalse) -> display 
      $ Str Lexemes.ifKw :+: cond 
      :+: Str Lexemes.thenKw :+: ifTrue 
      :+: Str Lexemes.elseKw :+: ifFalse
    CListFunc (CCons h t) -> display $ h :+: Str Lexemes.cons :+: t
    CListFunc CNil -> display $ Lexemes.openSquareBracket :+: Lexemes.closeSquareBracket
  display (CLetExpr (CLet defs rhs)) = display 
    $ Str Lexemes.letKw :+: defs :+: Str Lexemes.inKw :+:  rhs
  display (CConstructorExpr constructor) = display constructor
  display (COrElseExpr (COrElse left right _)) = display $ left :+: Str "<|>" :+: right
  display (CProdExpr (CProd name exprs)) = display $ Str name :+: SSV exprs
  display CBottomExpr = Lexemes.bottom

instance Display CVar where
  display (CVar name _) = name

instance Display CConst where
  display (CNumConst num) = case num of
    CInt i -> display i
    CFloat f -> display f
    CDouble d -> display d
  display (CCharConst (CChar ch)) = printf "'%s'" $ display ch
  display (CBoolConst CTrue) = Lexemes.true
  display (CBoolConst CFalse) = Lexemes.false
  display CUnitConst = Lexemes.unit

instance Display CConstructor where
  display (CConstructor name _) = name

instance Display CPat where
  display (CVarPat var) = display var
  display (CConstPat const') = display const'
  display (CConstructorPat constructor pats) = display $ constructor :+: SSV pats

instance Display CDefNel where
  display (CDefNel defs) = intercalate [Lexemes.semicolon, Lexemes.newline]
    $ NonEmpty.toList $ display <$> defs

instance Display CDef where
  display (CDef name rhs _) = display $ name :+: Lexemes.equals :+: rhs

instance Display CInfixOp where
  display (CArithOp arith) = case arith of
    CPlus -> [Lexemes.plus]
    CMinus -> [Lexemes.minus]
    CTimes -> [Lexemes.times]
    CDivide -> [Lexemes.divide]
    CModulo -> [Lexemes.modulo]
  display (CCompOp comp) = case comp of
    CEqEq -> Lexemes.equalsEquals
    CNotEq -> Lexemes.notEquals
    CLt -> [Lexemes.lessThan]
    CLte -> Lexemes.lessThanEquals
    CGt -> [Lexemes.greaterThan]
    CGte -> Lexemes.greaterThanEquals