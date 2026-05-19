module Mascheya.Core.Ast.CoreSpec where

import Mascheya.Core.Ast.Core (occursFree, Var (Var), Expr (VarExpr))
import Test.QuickCheck (quickCheck, Property, forAll)
import Mascheya.Core.Ast.Generators (genExpr, genVar)

propOccursFreeInSelf :: Property
propOccursFreeInSelf = forAll genVar $ \x@(Var xName) ->
    occursFree x (VarExpr $ Var xName)

propOccursFreeInOtherVars :: Property
propOccursFreeInOtherVars = forAll genVar $ \x@(Var xName) ->
    not $ occursFree x (VarExpr $ Var $ xName ++ "'") 