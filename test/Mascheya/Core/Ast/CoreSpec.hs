module Mascheya.Core.Ast.CoreSpec where

import Mascheya.Core.Ast.Core (occursFree, Var (Var), Expr (VarExpr, AppExpr), App (App), occursBound)
import Test.QuickCheck (quickCheck, Property, forAll, (==>))
import Mascheya.Core.Ast.Generators (genExpr, genVar, genApp)

propOccursFreeInSelf :: Property
propOccursFreeInSelf = forAll genVar $ \x@(Var xName) ->
    occursFree x (VarExpr $ Var xName)

propOccursFreeInOtherVars :: Property
propOccursFreeInOtherVars = forAll genVar $ \x@(Var xName) ->
    not $ occursFree x (VarExpr $ Var $ xName ++ "'") 

propOccursFreeInApp :: Property
propOccursFreeInApp = forAll genVarAndApp $ \(x, app@(App f e)) ->
    occursFree x (AppExpr app) ==> not (occursBound x f) || not (occursBound x e)
    where genVarAndApp = (,) <$> genVar <*> genApp