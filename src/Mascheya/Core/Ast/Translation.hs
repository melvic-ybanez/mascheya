module Mascheya.Core.Ast.Translation where

import Mascheya.Core.Result (Result)
import Mascheya.Core.Ast.Source 
import Mascheya.Core.Ast.Core 
import qualified Mascheya.Core.Result as Result
import Data.List.NonEmpty (NonEmpty((:|)))
import Mascheya.Core.Display (Display(display))
import qualified Mascheya.Core.Ast.Source as Source

translateExpr :: SExpr -> Result CExpr
translateExpr (SLitExpr lit) = Result.succeed $ CConstExpr $ case lit of
  SNumLit numlit -> CNumConst $ case numlit of 
    SIntNum (SInt int) ->  CInt int
    SFloatNum (SFloat float) -> CFloat float
    SDoubleNum (SDouble double) -> CDouble double
  SCharLit (SChar ch) -> CCharConst $ CChar ch
  SBoolLit STrue -> CBoolConst CTrue 
  SBoolLit SFalse -> CBoolConst CFalse
translateExpr (SVarExpr (SVar name line')) = Result.succeed $ CVarExpr $ CVar name line'
translateExpr (SAppExpr (SApp callable' (h :| t) loc)) = do
  cCallable <- translateExpr callable'
  cH <- translateExpr h
  rest <- sequence $ translateExpr <$> t
  let init' = CAppExpr $ CApp cCallable cH (display callable') loc
  let mkCExpr acc expr = CAppExpr $ CApp acc expr (display callable') loc
  return $ foldl' mkCExpr init' rest 
translateExpr (SInfixExpr (SInfix arg1 op arg2)) = 
  translateExpr $ SAppExpr $ SApp (SVarExpr op) (arg1 :| [arg2]) (Source.varLoc op)