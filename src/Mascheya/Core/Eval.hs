{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

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
    where eval' (ArithFunc anyArith) = eval'' anyArith
            where eval'' (AnyArith Plus a b) = evalArith a b (+)
                  eval'' (AnyArith Minus a b) = evalArith a b (-)
                  eval'' (AnyArith Times a b) = evalArith a b (*)

                  -- we are not using `evalArith` for division due to the edge case for ints
                  eval'' (AnyArith Divide (CInt a) (CInt b)) = eval $ mkInt $ a `div` b
                  eval'' (AnyArith Divide (CFloat a) (CFloat b)) = eval $ mkFloat $ a / b
                  eval'' (AnyArith Divide (CDouble a) (CDouble b)) = eval $ mkDouble $ a / b        
          eval' (IfFunc (If True expr _)) = eval expr
          eval' (IfFunc (If False _ expr)) = eval expr 
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ eval' const'
    where eval' (NumConst (AnyNum (CInt int))) = IntVal int
          eval' (NumConst (AnyNum (CFloat float))) = FloatVal float
          eval' (NumConst (AnyNum (CDouble double))) = DoubleVal double
          eval' (CharConst (CChar char)) = CharVal char
    

evalArith :: Numeric c -> Numeric c -> (forall a. Num a => (a -> a -> a)) -> Env Value -> Out
evalArith (CInt a) (CInt b) f = eval $ mkInt $ f a b
evalArith (CFloat a) (CFloat b) f = eval $ mkFloat $ f a b
evalArith (CDouble a) (CDouble b) f = eval $ mkDouble $ f a b

force :: Value -> Out
force (ThunkVal (Thunk value')) = value'
force val = Result.succeed val