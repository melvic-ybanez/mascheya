module Mascheya.Core.Ast.Source where
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Result (Loc)

data Expr = VarExpr SVar | LiteralExpr Literal deriving Show

data SVar = SVar { varName :: String, loc :: Loc } deriving Show
data Literal = NumLit SNum | CharLit SChar | BoolLit SBool deriving Show

data SNum = IntLit SInt | FloatLit SFloat | DoubleLit SDouble deriving Show
data SBool = STrue | SFalse deriving Show

newtype SInt = SInt Int deriving Show
newtype SFloat = SFloat Float deriving Show
newtype SDouble = SDouble Double deriving Show

data SChar = SChar Char deriving Show

instance Display Expr where
  display (VarExpr (SVar name _)) = name
  display (LiteralExpr lit) = case lit of 
    NumLit num -> case num of
      IntLit (SInt int) -> display int
      FloatLit (SFloat float) -> display float
      DoubleLit (SDouble double) -> display double
    BoolLit STrue -> display True
    BoolLit SFalse -> display False
    CharLit (SChar ch) -> display ch
    