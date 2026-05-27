module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Ast.Core hiding (body)
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result (notAFunction, Failure (RuntimeError))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))

eval :: Expr -> Env Value -> Out
eval (VarExpr var) = (>>= force) . Env.lookup var
eval (LambdaExpr (Lambda param body)) = Result.succeed . FunctionVal . Function param body
eval (AppExpr (App func arg)) = runReaderT $ ReaderT (eval func) >>= handleFuncVal
    where handleFuncVal (FunctionVal closure) = ReaderT $ eval (body closure) . extendEnv closure
          handleFuncVal _ = ReaderT $ const $ Result.fail $ RuntimeError $ notAFunction func

          argThunk = Thunk . eval arg
          extendEnv (Function param _ env) oldEnv = Env.extend param (ThunkVal $ argThunk oldEnv) env
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ eval' const'
    where eval' (C.CInt int) = IntVal int
          eval' (C.CFloat float) = FloatVal float
          eval' (C.CDouble double) = DoubleVal double
          eval' (C.CChar char) = CharVal char
          eval' (C.CBool bool) = BoolVal bool

force :: Value -> Out
force (ThunkVal (Thunk value')) = value'
force val = Result.succeed val

