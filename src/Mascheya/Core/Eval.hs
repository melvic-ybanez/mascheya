module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value (Out, Value (..), Const (IntVal, FloatVal, DoubleVal, CharVal, BoolVal))
import qualified Mascheya.Core.Ast.Core as C
import Mascheya.Core.Ast.Core (Expr (..), App (App), Lambda (Lambda), Var (Var))
import Mascheya.Core.Eval.Env (Env, assign)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Reader
import Mascheya.Core.Token (lexeme)

eval :: Expr -> Env -> Out
eval (VarExpr var) = Env.lookup var
eval (AppExpr (App func arg)) = runReaderT $ do 
    evaluatedFunc <- ReaderT $ eval func 
    evalauatedArg <- ReaderT $ eval arg  
    ReaderT $ const $ Result.succeed $ AppVal evaluatedFunc evalauatedArg
eval (LambdaExpr (Lambda (Var param) body)) = \env ->
    Result.succeed $ LambdaVal $ \paramValue -> eval body $ assign (lexeme param) paramValue env
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ evalConst const'
    where evalConst (C.CInt int) = IntVal int 
          evalConst (C.CFloat float) = FloatVal float
          evalConst (C.CDouble double) = DoubleVal double
          evalConst (C.CChar char) = CharVal char
          evalConst (C.CBool bool) = BoolVal bool

