module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display), SSV (SSV))
import Mascheya.Core.Result (Loc)
import qualified Mascheya.Core.Lexemes as Lexemes
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.List (intercalate)

data SProg = SDefNelProg SDefNel | SExprProg SExpr deriving Show

data SExpr = SVarExpr SVar | SAppExpr SApp | SLitExpr SLit 
  | SInfixExpr SInfix | SLetExpr SLet | SLambdaExpr SLambda 
  | SConstructorExpr SConstructor deriving (Show, Eq)

-- | Lambda expression.
--
-- Note: A source-level lambda can have multiple params, but it gets converted into
-- a curried single-param lambda during the translation phase
data SLambda = SLambda (NonEmpty SPat) SExpr deriving (Show, Eq)

data SVar = SVar { varName :: String, varLoc :: Loc } deriving (Show, Eq, Ord)
data SLit = SNumLit SNum | SCharLit SChar | SBoolLit SBool deriving (Show, Eq)

data SApp = SApp { callable :: SExpr, args :: NonEmpty SExpr, appLoc :: Loc } deriving (Show, Eq)

data SNum = SIntNum SInt | SFloatNum SFloat | SDoubleNum SDouble deriving (Show, Eq)
data SBool = STrue | SFalse deriving (Show, Eq)

newtype SInt = SInt Int deriving (Show, Eq)
newtype SFloat = SFloat Float deriving (Show, Eq)
newtype SDouble = SDouble Double deriving (Show, Eq)
newtype SChar = SChar Char deriving (Show, Eq)

data SInfix = SInfix SExpr SVar SExpr deriving (Show, Eq)

data SLet = SLet SDefNel SExpr deriving (Show, Eq)

data SDef = SDef {
  defName :: SVar,
  defParams :: [SPat],
  defRhs :: SExpr
} deriving (Show, Eq)

data SProd = SProd String [SExpr] deriving Show

data SConstructor = SConstructor String Loc deriving (Show, Eq)

newtype SDefNel = SDefNel (NonEmpty SDef) deriving (Show, Eq)

data SPat = SVarPat SVar | SLitPat SLit | SConstructorPat SConstructor [SPat] deriving (Show, Eq)

instance Display SProg where
  display (SDefNelProg defNel) = display defNel
  display (SExprProg expr) = display expr

instance Display SExpr where
  display (SVarExpr var) = display var
  display (SLitExpr lit) = display lit
  display (SAppExpr (SApp func args' _)) = display func ++ displayArgs args'
    where 
      displayArgs (h :| t) = [Lexemes.space] ++ display h ++ case t of
        [] -> ""
        t' -> displayArgs $ fromList t'
  display (SInfixExpr (SInfix arg1 op arg2)) = 
    display $ SSV [arg1, SVarExpr op, arg2]
  display (SLetExpr (SLet defs expr)) = 
    Lexemes.letKw ++ newlineIndent ++ display defs ++ [Lexemes.newline] ++ Lexemes.inKw 
    ++ [Lexemes.space] ++ display expr
    where 
      newlineIndent = [Lexemes.newline, Lexemes.space, Lexemes.space]
  display (SLambdaExpr (SLambda params body)) = display 
    (Lexemes.lambdaSymbol, (SSV $ toList params, body))
  display (SConstructorExpr constr) = display constr

instance Display SVar where
  display (SVar name _) = name

instance Display SLit where
  display (SNumLit num) = case num of
      SIntNum (SInt int) -> display int
      SFloatNum (SFloat float) -> display float
      SDoubleNum (SDouble double) -> display double
  display (SBoolLit STrue) = display True
  display (SBoolLit SFalse) = display False
  display (SCharLit (SChar ch)) = "'" ++ display ch ++ "'"

instance Display SDef where
  display (SDef name params rhs) = display (name, (SSV params, (Lexemes.equals, rhs)))

instance Display SDefNel where
  display (SDefNel defs) = intercalate 
    [Lexemes.semicolon, Lexemes.newline] $ display <$> toList defs 

instance Display SPat where
  display (SVarPat var) = display var
  display (SLitPat lit) = display lit
  display (SConstructorPat constructor args') = display constructor
    ++ [Lexemes.space] ++ (display $ SSV args')

instance Display SConstructor where
  display (SConstructor name _) = name