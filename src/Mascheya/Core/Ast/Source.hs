module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display), SSV (SSV))
import Mascheya.Core.Result (Loc)
import qualified Mascheya.Core.Lexemes as Lexemes
import Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)
import Data.List (intercalate)

data SProg = SDefProg SDef | SExprProg SExpr deriving Show

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

data SLet = SLet (NonEmpty SDef) SExpr deriving Show

data SDef = SDef SExpr SExpr Loc deriving Show

instance Display SProg where
  display (SDefProg def) = display def
  display (SExprProg expr) = display expr

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
    display $ SSV [arg1, SVarExpr op, arg2]
  display (SLetExpr (SLet defs expr)) = 
    Lexemes.letKw ++ newlineIndent ++ displayDefs ++ [Lexemes.newline] ++ Lexemes.inKw 
    ++ [Lexemes.space] ++ display expr
    where 
      newlineIndent = Lexemes.newline : [Lexemes.space, Lexemes.space]
      displayDefs = intercalate newlineIndent $ fmap display $ display $ SSV $ toList defs

instance Display SVar where
  display (SVar name _) = name

instance Display SDef where
  display (SDef rhs lhs _) = display $ SSV [rhs, lhs]