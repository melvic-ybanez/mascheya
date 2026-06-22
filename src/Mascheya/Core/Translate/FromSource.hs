module Mascheya.Core.Translate.FromSource where

import Mascheya.Core.Result (Result)
import Mascheya.Core.Ast.Source 
import Mascheya.Core.Ast.Core 
import qualified Mascheya.Core.Result as Result
import Data.List.NonEmpty (NonEmpty((:|)))
import Mascheya.Core.Display (Display(display))
import qualified Mascheya.Core.Ast.Source as Source
import Mascheya.Core.Ast.Enriched (EExpr (ECoreExpr, ELetExpr), ELet (ELet))
import Mascheya.Core.Translate.FromEnriched (fromEExpr)

fromSExpr :: SExpr -> Result EExpr
fromSExpr (SLitExpr lit) = succeedCore $ CConstExpr $ case lit of
  SNumLit numlit -> CNumConst $ case numlit of 
    SIntNum (SInt int) ->  CInt int
    SFloatNum (SFloat float) -> CFloat float
    SDoubleNum (SDouble double) -> CDouble double
  SCharLit (SChar ch) -> CCharConst $ CChar ch
  SBoolLit STrue -> CBoolConst CTrue 
  SBoolLit SFalse -> CBoolConst CFalse
fromSExpr (SVarExpr sVar) = ECoreExpr . CVarExpr <$> fullTranslateVar sVar
fromSExpr (SAppExpr (SApp callable' (h :| t) loc)) = do
  cCallable <- fullTranslate callable'
  cH <- fullTranslate h
  rest <- sequence $ fullTranslate <$> t
  let init' = CAppExpr $ CApp cCallable cH (display callable') loc
  let mkCExpr acc expr = CAppExpr $ CApp acc expr (display callable') loc
  succeedCore $ foldl' mkCExpr init' rest 
fromSExpr (SInfixExpr (SInfix arg1 op arg2)) =
  fromSExpr $ SAppExpr $ SApp (SVarExpr op) (arg1 :| [arg2]) (Source.varLoc op)
fromSExpr (SLetExpr (SLet v b e)) = do
  ev <- fullTranslateVar v
  eb <- fromSExpr b
  ee <- fromSExpr e
  return $ ELetExpr $ ELet ev eb ee

succeedCore :: CExpr -> Result EExpr
succeedCore = Result.succeed . ECoreExpr

fullTranslate :: SExpr -> Result CExpr
fullTranslate = (>>= fromEExpr) . fromSExpr

fullTranslateVar :: SVar -> Result CVar
fullTranslateVar (SVar name line') = Result.succeed $ CVar name line'