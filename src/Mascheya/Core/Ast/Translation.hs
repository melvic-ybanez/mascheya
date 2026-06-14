module Mascheya.Core.Ast.Translation where

import qualified Mascheya.Core.Ast.Source as S
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result, succeed)
import Mascheya.Core.Ast.Source 
import Mascheya.Core.Ast.Core 

translateExpr :: S.Expr -> Result C.Expr
translateExpr (LiteralExpr lit) = succeed $ ConstExpr $ case lit of
  NumLit numlit -> NumConst $ case numlit of 
    IntLit (SInt int) ->  CInt int
    FloatLit (SFloat float) -> CFloat float
    DoubleLit (SDouble double) -> CDouble double
  CharLit (SChar ch) -> CharConst $ CChar ch
  BoolLit STrue -> BoolConst CTrue 
  BoolLit SFalse -> BoolConst CFalse