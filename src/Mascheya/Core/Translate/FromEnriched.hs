module Mascheya.Core.Translate.FromEnriched where
  
import Mascheya.Core.Ast.Core 
import Mascheya.Core.Result
import Mascheya.Core.Ast.Enriched (EExpr (ELetExpr, ECoreExpr), ELet (ELet))

fromEExpr :: EExpr -> Result CExpr
fromEExpr (ECoreExpr core) = return core
fromEExpr (ELetExpr (ELet var body' expr)) = do
  coreBody <- fromEExpr body'
  coreExpr <- fromEExpr expr
  let callable' = CLambdaExpr $ CLambda var coreExpr
  return $ CAppExpr $ CApp callable' (CRec coreBody) "<let-expression>" (varLoc var)