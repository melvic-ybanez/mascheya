module Mascheya.Core.Ast.Generators where

import Mascheya.Core.Ast.Core (Expr (..), Var (VarData), Const)
import Test.QuickCheck (Arbitrary, oneof, Gen, sized, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import qualified Mascheya.Core.Ast.Core as Core

genExpr :: Gen Expr
genExpr = sized genExpr'
    where genExpr' 0 = Const <$> genConst
          genExpr' n = frequency [
                (1, Const <$> genConst),
                (2, Var <$> genVar),
                (3, Lambda <$> genVar <*> genExpr),
                (4, Application <$> genExpr <*> genExpr)
            ]

genVar :: Gen Var
genVar = VarData <$> arbitrary

genConst :: Gen Const
genConst = Core.Int <$> arbitrary
          