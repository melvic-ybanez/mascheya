module Mascheya.Core.Translate.FromEnriched where
  
import Mascheya.Core.Ast.Core 
import Mascheya.Core.Result
import Mascheya.Core.Ast.Enrichments (ELet (ELet), EExpr (ELetExpr))
import qualified Mascheya.Core.Result as Result

fromEExpr :: EExpr -> Result CExpr
fromEExpr (ELetExpr (ELet var body' expr)) = 
  Result.succeed $ CAppExpr $ CApp callable' (CRec body') "<let-expression>" (varLoc var)
  where 
    callable' = CLambdaExpr $ CLambda var expr