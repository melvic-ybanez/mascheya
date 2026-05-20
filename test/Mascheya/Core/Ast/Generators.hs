module Mascheya.Core.Ast.Generators where

import Mascheya.Core.Ast.Core
import Test.QuickCheck (Arbitrary, oneof, Gen, sized, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

genExpr :: Gen Expr
genExpr = sized genExpr'
    where genExpr' 0 = ConstExpr <$> genConst
          genExpr' n = frequency [
                (1, ConstExpr <$> genConst),
                (2, VarExpr <$> genVar),
                (3, LambdaExpr <$> (Lambda <$> genVar <*> genExpr)),
                (4, AppExpr <$> genApp)
                -- TODO: Add item for the builtin functions
            ]

genVar :: Gen Var
genVar = Var <$> arbitrary

genConst :: Gen Const
genConst = CInt <$> arbitrary

genApp :: Gen App
genApp = App <$> genExpr <*> genExpr