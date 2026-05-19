module Mascheya.Core.Ast.CoreSpec where

import Mascheya.Core.Ast.Core (occursFree, Var (VarData), Expr (Var))
import Test.QuickCheck (quickCheck, Property, forAll)
import Mascheya.Core.Ast.Generators (genExpr, genVar)

propOccursFreeInSelf :: Property
propOccursFreeInSelf = forAll genVar $ \x@(VarData xName) ->
    occursFree x (Var $ VarData xName)

