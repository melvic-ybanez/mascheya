module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Result (Loc)
import qualified Mascheya.Core.Lexemes as Lexemes
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.List (intercalate)
import qualified Mascheya.Core.Lexemes as Lexems

data SExpr = SVarExpr SVar | SAppExpr SApp | SLitExpr SLit 
  | SInfixExpr SInfix | SLetExpr SLet deriving Show

data SVar = SVar { varName :: String, varLoc :: Loc } deriving Show
data SLit = SNumLit SNum | SCharLit SChar | SBoolLit SBool deriving Show

data SApp = SApp { callable :: SExpr, args :: NonEmpty SExpr, appLoc :: Loc } deriving Show

data SNum = SIntNum SInt | SFloatNum SFloat | SDoubleNum SDouble deriving Show
data SBool = STrue | SFalse deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show

data SInfix = SInfix SExpr SVar SExpr deriving Show

data SLet = SLet SVar SExpr SExpr deriving Show

instance Display SExpr where
  display (SVarExpr var) = display var
  display (SLitExpr lit) = case lit of 
    SNumLit num -> case num of
      SIntNum (SInt int) -> display int
      SFloatNum (SFloat float) -> display float
      SDoubleNum (SDouble double) -> display double
    SBoolLit STrue -> display True
    SBoolLit SFalse -> display False
    SCharLit (SChar ch) -> display ch
  display (SAppExpr (SApp func args' _)) = display func ++ displayArgs args'
    where 
      displayArgs (h :| t) = [Lexemes.space] ++ display h ++ case t of
        [] -> ""
        t' -> displayArgs $ fromList t'
  display (SInfixExpr (SInfix arg1 op arg2)) = 
    intercalate [Lexemes.space] [display arg1, display op, display arg2]
  display (SLetExpr (SLet var body expr)) = 
    intercalate [Lexemes.space] [Lexems.letKw, display var, [Lexemes.equals], 
      display body, Lexemes.newline : Lexemes.inKw, display expr]

instance Display SVar where
  display (SVar name _) = name