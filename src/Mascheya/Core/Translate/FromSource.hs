module Mascheya.Core.Translate.FromSource where

import Mascheya.Core.Ast.Source 
import Mascheya.Core.Ast.Core 
import qualified Mascheya.Core.Result as Result
import Data.List.NonEmpty (NonEmpty((:|)), sortOn, group)
import Mascheya.Core.Display (Display(display))
import qualified Mascheya.Core.Ast.Source as Source
import Mascheya.Core.Result (
    Result, Failure (TranslationError), TranslationError (UnableToTranslate)
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.List (intercalate)
import qualified Mascheya.Core.Lexemes as Lexemes
import qualified Mascheya.Core.Ast.Core as Core
import Control.Arrow ((>>>))

toCoreProg :: SProg -> Result CProg
toCoreProg (SDefNelProg sDefNel) = CDefNelProg <$> toCoreDefNel sDefNel
toCoreProg (SExprProg sExpr) = CExprProg <$> toCoreExpr sExpr

toCoreExpr :: SExpr -> Result CExpr
toCoreExpr (SLitExpr lit) = Result.succeed $ CConstExpr $ toCoreConst lit
toCoreExpr (SVarExpr sVar) = Result.succeed $ CVarExpr $ toCoreVar sVar
toCoreExpr (SAppExpr (SApp callable' (h :| t) loc)) = do
  cCallable <- toCoreExpr callable'
  cH <- toCoreExpr h
  rest <- sequence $ toCoreExpr <$> t
  let init' = CAppExpr $ CApp cCallable cH source loc
      mkExpr acc expr = CAppExpr $ CApp acc expr source loc
      source = display callable'
  Result.succeed $ foldl' mkExpr init' rest
toCoreExpr (SInfixExpr (SInfix arg1 op arg2)) =
  toCoreExpr $ SAppExpr $ SApp (SVarExpr op) (arg1 :| [arg2]) (Source.varLoc op)
toCoreExpr (SLetExpr (SLet sDefNel expr)) = do
  (CDefNel mergedDefs) <- toCoreDefNel sDefNel
  cExpr <- toCoreExpr expr
  return $ CLetExpr $ CLet (CDefNel mergedDefs) cExpr
toCoreExpr (SLambdaExpr (SLambda params body')) = do
  cExpr <- toCoreExpr body'
  let cParams = toCorePat <$> params
  return $ fromLambdaDetails (NonEmpty.toList cParams) cExpr
toCoreExpr (SConstructorExpr constr) = Result.succeed $ CConstructorExpr $ toCoreConstructor constr

toCoreDefNel :: SDefNel -> Result CDefNel
toCoreDefNel (SDefNel defNel) = do
  let defGroups = group $ sortOn Source.defName defNel
      paramLens = fmap (fmap (length . defParams)) defGroups
  perGroupParamLens <- if all ((\(h :| t) -> all (== h) t)) paramLens
    then Result.succeed $ fmap NonEmpty.head paramLens 
    else Result.fail $ TranslationError $ UnableToTranslate "Number of params are not the same"
      $ Source.varLoc $ Source.defName $ NonEmpty.head defNel 
  cDefGroups <- sequence $ sequence . fmap toCoreDef <$> defGroups
  let multiDefToOrElse (dh :| dt) params = foldl' (combine params) (mkApp params dh) dt
        where 
          combine params' orElse cDef@(CDef (CVar _ at) _ _) = 
            COrElseExpr $ COrElse orElse (mkApp params' cDef) at
          mkApp (ph :| pt) (CDef (CVar name at) rhs _) = CAppExpr $ foldl' 
            (\app param' -> CApp (CAppExpr app) param' name at)
            (CApp rhs (CVarExpr ph) name at) 
            $ fmap CVarExpr pt
      
      multiDefToSingle (def :| [], []) = Result.succeed def
      multiDefToSingle ((CDef (CVar varName' varLoc') _ _) :| _, []) = Result.fail
        $ TranslationError $ UnableToTranslate ("Multiple definitions for " ++ varName')
        $ varLoc'
      multiDefToSingle (defs@((CDef name _ _) :| _), params) = do
        let lambdaBody' = multiDefToOrElse defs $ NonEmpty.fromList params
            paramPats = fmap CVarPat params
            lambda = fromLambdaDetails paramPats lambdaBody'
        return $ CDef name lambda
          $ intercalate [Lexemes.newline] $ NonEmpty.toList $ defSource <$> defs
      
      mergedDefs = NonEmpty.fromList $ multiDefToSingle <$> defParamsMap
        where 
          lenToParams len locs = fmap (\(l, at) -> CVar ('a' : show l) at) 
            $ zip [1..len] (NonEmpty.toList locs)
          defParamsMap = do
            (cDefs, paramLen) <- zip cDefGroups perGroupParamLens
            let locs = fmap (Core.defName >>> Core.varLoc) cDefs
            return (cDefs, lenToParams paramLen locs)
  fmap CDefNel $ sequence mergedDefs

toCoreConst :: SLit -> CConst
toCoreConst lit = case lit of
  SNumLit numlit -> CNumConst $ case numlit of 
    SIntNum (SInt int) ->  CInt int
    SFloatNum (SFloat float) -> CFloat float
    SDoubleNum (SDouble double) -> CDouble double
  SCharLit (SChar ch) -> CCharConst $ CChar ch
  SBoolLit STrue -> CBoolConst CTrue 
  SBoolLit SFalse -> CBoolConst CFalse

toCoreVar :: SVar -> CVar
toCoreVar (SVar name line') = CVar name line'

toCoreConstructor :: SConstructor -> CConstructor
toCoreConstructor (SConstructor name loc) = CConstructor name loc

toCorePat :: SPat -> CPat
toCorePat (SVarPat pat) = CVarPat $ toCoreVar pat
toCorePat (SLitPat pat) = CConstPat $ toCoreConst pat
toCorePat (SConstructorPat constructor pats) = 
  CConstructorPat (toCoreConstructor constructor) $ toCorePat <$> pats

toCoreDef :: SDef -> Result CDef
toCoreDef def@(SDef name params rhs) = do
  cRhs <- toCoreExpr rhs
  let cParams = toCorePat <$> params
      mkLambdaExpr param' acc = CLambdaExpr $ CLambda param' acc  
  return $ CDef (toCoreVar name) (foldr mkLambdaExpr cRhs cParams) (display def)