module Mascheya.Core.Ast.Translation where

import qualified Mascheya.Core.Ast.Source as S
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result, succeed)
import Mascheya.Core.Ast.Source (Expr(Literal), Literal(SInt))

translateExpr :: S.Expr -> Result C.Expr
translateExpr (Literal (SInt value)) = 
  succeed $ C.ConstExpr $ C.NumConst $ C.CInt $ fromIntegral value