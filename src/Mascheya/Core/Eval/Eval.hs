module Mascheya.Core.Eval.Eval where
import Mascheya.Core.Ast.Core (Expr (Constant))
import Mascheya.Core.Eval.Value (Value (Number))
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Result (Result, succeed)

type Out = Result Value

evalExpr :: Expr -> Out
evalExpr (Constant (C.Number value)) = succeed $ Number value