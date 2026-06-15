module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Result (Loc)
import qualified Mascheya.Core.Lexemes as Lexemes
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)

data SExpr = SVarExpr SVar | SAppExpr SApp | SLitExpr SLit deriving Show

data SVar = SVar { varName :: String, varLoc :: Loc } deriving Show
data SLit = SNumLit SNum | SCharLit SChar | SBoolLit SBool deriving Show

data SApp = SApp { callable :: SExpr, args :: NonEmpty SExpr, appLoc :: Loc } deriving Show

data SNum = SIntNum SInt | SFloatNum SFloat | SDoubleNum SDouble deriving Show
data SBool = STrue | SFalse deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show

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
    