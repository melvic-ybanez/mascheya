module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value (Out, Value (..), Thunk (Delayed, Evaluated), Const (..), Closure (Closure), body)
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Ast.Core (Expr (..), App (App), Lambda (Lambda))
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result (notAFunction, Failure (RuntimeError))

eval :: Expr -> Env Thunk -> Out
eval (VarExpr var) = (>>= force) . Env.lookup var
eval (LambdaExpr (Lambda param body)) = Result.succeed . ClosureVal . Closure param body
eval (AppExpr (App func arg)) = \env -> eval func env 
    >>= handleFuncVal 
    >>= \funcVal -> eval (body funcVal) (extendedEnv funcVal env)
    where handleFuncVal (ClosureVal closure) = Result.succeed closure
          handleFuncVal _ = Result.fail $ RuntimeError $ notAFunction func
          
          argThunk = Delayed arg
          extendedEnv (Closure param _ env) oldEnv = Env.extend param (argThunk oldEnv) env
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ evalConst const'
    where evalConst (C.CInt int) = IntVal int 
          evalConst (C.CFloat float) = FloatVal float
          evalConst (C.CDouble double) = DoubleVal double
          evalConst (C.CChar char) = CharVal char
          evalConst (C.CBool bool) = BoolVal bool

force :: Thunk -> Out
force (Evaluated value) = Result.succeed value
force (Delayed expr env) = eval expr env

