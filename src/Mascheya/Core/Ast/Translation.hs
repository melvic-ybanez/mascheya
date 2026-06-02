module Mascheya.Core.Ast.Translation where

import qualified Mascheya.Core.Ast.Source as S
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result, succeed)

translateExpr :: S.Expr -> Result C.Expr
translateExpr (S.Literal (S.SInt value)) = 
  succeed $ C.ConstExpr $ C.NumConst $ C.CInt $ fromIntegral value