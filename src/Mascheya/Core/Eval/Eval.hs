module Mascheya.Core.Eval.Eval where
import Mascheya.Core.Ast.Core (Expr (ConstExpr))
import Mascheya.Core.Eval.Value (Value)
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result, succeed)
import qualified Mascheya.Core.Eval.Value as Value

type Out = Result Value

evalExpr :: Expr -> Out
evalExpr (ConstExpr (C.CInt value)) = succeed $ Value.Int value