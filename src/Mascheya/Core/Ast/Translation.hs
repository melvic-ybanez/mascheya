module Mascheya.Core.Ast.Translation where

import qualified Mascheya.Core.Ast.Source as S
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result)
import Mascheya.Core.Ast.Source 
import Mascheya.Core.Ast.Core 
import qualified Mascheya.Core.Result as Result

translateExpr :: S.Expr -> Result C.Expr
translateExpr (LiteralExpr lit) = Result.succeed $ ConstExpr $ case lit of
  NumLit numlit -> NumConst $ case numlit of 
    IntLit (SInt int) ->  CInt int
    FloatLit (SFloat float) -> CFloat float
    DoubleLit (SDouble double) -> CDouble double
  CharLit (SChar ch) -> CharConst $ CChar ch
  BoolLit STrue -> BoolConst CTrue 
  BoolLit SFalse -> BoolConst CFalse
translateExpr (S.VarExpr (SVar name line')) = Result.succeed $ C.VarExpr $ CVar name line'