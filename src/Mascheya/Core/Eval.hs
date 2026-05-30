{-# LANGUAGE RankNTypes #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import Mascheya.Core.Eval.Env (Env)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT))
import Mascheya.Core.Token (Token(lexeme))

eval :: Expr -> Env Value -> Out
eval (VarExpr (Var token)) = (>>= force) . maybe error' Right . Env.lookup (lexeme token)
    where error' = Result.fail $ RuntimeError $ Result.undefinedVar token
eval (LambdaExpr (Lambda (Var token) body')) = Result.succeed . FunctionVal . Function (lexeme token) body'
eval (AppExpr (App func arg')) = runReaderT $ ReaderT (eval func) >>= handleFuncVal
    where handleFuncVal (FunctionVal closure@(Function param' _ env)) = 
            ReaderT $ eval (body closure) . extendEnv
            where argThunk = Thunk . eval arg'
                  extendEnv oldEnv = Env.extend param' (ThunkVal $ argThunk oldEnv) env
          handleFuncVal _ = ReaderT $ const $ Result.fail $ RuntimeError $ notAFunction func
eval (BuiltinFuncExpr builtin _) = eval' builtin
    where eval' (ArithFunc anyArith) = eval'' anyArith
            where eval'' (Arith Plus a b) = evalArith a b (+) (+)
                  eval'' (Arith Minus a b) = evalArith a b (-) (-)
                  eval'' (Arith Times a b) = evalArith a b (*) (*)
                  eval'' (Arith Divide a b) = evalArith a b div (/)

                  evalArith :: Expr -> Expr -> (Int -> Int -> Int) -> 
                    (forall a. Fractional a => a -> a -> a) -> Env Value -> Out
                  evalArith a b f g = runReaderT $ do
                    aVal <- ReaderT $ eval a
                    bVal <- ReaderT $ eval b
                    ReaderT $ case (aVal, bVal) of
                        ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ mkInt $ f i1 i2
                        ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> eval $ mkFloat $ g f1 f2
                        ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> eval $ mkDouble $ g d1 d2
                        (_, _) -> const $ Result.fail $ InternalError $ TypecheckingFailed
          eval' (IfFunc (If True expr _)) = eval expr
          eval' (IfFunc (If False _ expr)) = eval expr
          eval' (ListFunc Nil) = const $ Result.succeed $ ListVal NilVal
          eval' (ListFunc (Cons h t)) = Result.succeed . ListVal . ConsVal h t
eval (ConstExpr const') = const $ Result.succeed $ ConstVal $ eval' const'
    where eval' (NumConst (CInt int)) = IntVal int
          eval' (NumConst (CFloat float)) = FloatVal float
          eval' (NumConst (CDouble double)) = DoubleVal double
          eval' (CharConst (CChar char)) = CharVal char

force :: Value -> Out
force (ThunkVal (Thunk value')) = value'
force val = Result.succeed val