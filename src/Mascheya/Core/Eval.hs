{-# LANGUAGE RankNTypes #-}
module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result (notAFunction, Failure (RuntimeError))
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Mascheya.Core.Token (Token(lexeme))

eval :: Expr -> Env Value -> Out
eval (VarExpr (Var token)) = (>>= force) . (maybe error' Right . Env.lookup (lexeme token))
    where error' = Result.fail $ RuntimeError $ Result.undefinedVar token
eval (LambdaExpr (Lambda (Var token) body')) = Result.succeed . FunctionVal . Function (lexeme token) body'
eval (AppExpr (App func arg')) = runReaderT $ ReaderT (eval func) >>= handleFuncVal
    where handleFuncVal (FunctionVal closure@(Function param' _ env)) = ReaderT $ eval (body closure) . extendEnv
            where argThunk = Thunk . eval arg'
                  extendEnv oldEnv = Env.extend param' (ThunkVal $ argThunk oldEnv) env
          handleFuncVal _ = ReaderT $ const $ Result.fail $ RuntimeError $ notAFunction func
eval (BuiltinFuncExpr builtin _) = eval' builtin
    where eval' (ArithFunc Plus a b) = evalArith a b $ \a -> \b -> a + b
          eval' (ArithFunc Minus a b) = evalArith a b $ \a -> \b -> a - b
          eval' (ArithFunc Times a b) = evalArith a b $ \a -> \b -> a * b

          -- we are not using `evalArith` for division due to the edge case for ints
          eval' (ArithFunc Divide (CInt a) (CInt b)) = eval $ mkInt $ a `div` b
          eval' (ArithFunc Divide (CFloat a) (CFloat b)) = eval $ mkFloat $ a / b
          eval' (ArithFunc Divide (CDouble a) (CDouble b)) = eval $ mkDouble $ a / b
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ eval' const'
    where eval' (NumConst (CInt int)) = IntVal int
          eval' (NumConst (CFloat float)) = FloatVal float
          eval' (NumConst (CDouble double)) = DoubleVal double
          eval' (CChar char) = CharVal char
          eval' (CBool bool) = BoolVal bool

evalArith :: Numeric -> Numeric -> (forall a. Num a => (a -> a -> a)) -> Env Value -> Out
evalArith (CInt a) (CInt b) f = eval $ mkInt $ f a b
evalArith (CFloat a) (CFloat b) f = eval $ mkFloat $ f a b
evalArith (CDouble a) (CDouble b) f = eval $ mkDouble $ f a b

force :: Value -> Out
force (ThunkVal (Thunk value')) = value'
force val = Result.succeed val