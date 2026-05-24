module Mascheya.Core.Eval.Eval where

import Mascheya.Core.Eval.Value (Out, Value (IntValue, AppValue))
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Ast.Core (Expr (VarExpr, ConstExpr, AppExpr), App (App))
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Reader

eval :: Expr -> Env -> Out
eval (VarExpr var) = Env.lookup var
eval (ConstExpr (C.CInt value)) = \_ -> Result.succeed $ IntValue value
eval (AppExpr (App func arg)) = runReaderT $ do 
    evaluatedFunc <- ReaderT $ eval func 
    evalauatedArg <- ReaderT $ eval arg  
    ReaderT $ \_ -> Result.succeed $ AppValue evaluatedFunc evalauatedArg 