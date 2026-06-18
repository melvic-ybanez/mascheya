module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Result (Loc (Loc))
import qualified Mascheya.Core.Lexemes as Lexemes
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)

data SExpr = SVarExpr SVar | SAppExpr SApp | SLitExpr SLit | SBuiltinFuncExpr SBuiltinFunc deriving Show

data SVar = SVar { varName :: String, varLoc :: Loc } deriving Show
data SLit = SNumLit SNum | SCharLit SChar | SBoolLit SBool deriving Show

data SApp = SApp { callable :: SExpr, args :: NonEmpty SExpr, appLoc :: Loc } deriving Show

data SNum = SIntNum SInt | SFloatNum SFloat | SDoubleNum SDouble deriving Show
data SBool = STrue | SFalse deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show

data SBuiltinFunc = SArithFunc SArith deriving Show

data SArith = SArith SArithKind SExpr SExpr deriving Show

data SArithKind = SPlus | SMinus | STimes | SDivide | SModulo deriving Show

instance Display SExpr where
  display (SVarExpr (SVar name _)) = name
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
  display (SBuiltinFuncExpr builtin) = case builtin of
    SArithFunc (SArith op expr1 expr2) -> 
      display $ SAppExpr $ SApp var args' loc
      where 
        var = SVarExpr $ SVar [opName] loc
        args' = expr1 :| [expr2]
        loc = Loc 0 -- just a dummy value. We don't need to display this anyway
        opName = case op of
          SPlus -> Lexemes.plus
          SMinus -> Lexemes.minus
          STimes -> Lexemes.times
          SDivide -> Lexemes.divide
          SModulo -> Lexemes.modulo
    