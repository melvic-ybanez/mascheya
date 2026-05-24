module Mascheya.Core.Eval.Eval where

import Mascheya.Core.Eval.Value (Out, Value (IntValue, AppValue, LambdaValue))
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Ast.Core (Expr (..), App (App), Lambda (Lambda), Var (Var))
import Mascheya.Core.Eval.Env (Env, assign)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Reader
import Mascheya.Core.Token (lexeme)

eval :: Expr -> Env -> Out
eval (VarExpr var) = Env.lookup var
eval (ConstExpr (C.CInt value)) = const $ Result.succeed $ IntValue value
eval (AppExpr (App func arg)) = runReaderT $ do 
    evaluatedFunc <- ReaderT $ eval func 
    evalauatedArg <- ReaderT $ eval arg  
    ReaderT $ const $ Result.succeed $ AppValue evaluatedFunc evalauatedArg
eval (LambdaExpr (Lambda (Var param) body)) = \env ->
    Result.succeed $ LambdaValue $ \paramValue -> eval body $ assign (lexeme param) paramValue env