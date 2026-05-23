module Mascheya.Core.Eval.Eval where
import Mascheya.Core.Eval.Value (Out)
import qualified Mascheya.Core.Ast.Core as C
import qualified Mascheya.Core.Eval.Value as Value
import Mascheya.Core.Ast.Core (Expr (VarExpr, ConstExpr))
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env

evalExpr :: Expr -> Env -> Out
evalExpr (VarExpr var) = Env.lookup var
evalExpr (ConstExpr (C.CInt value)) = \_ -> Result.succeed $ Value.Int value