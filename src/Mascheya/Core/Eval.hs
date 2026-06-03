{-# LANGUAGE RankNTypes #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT) )
import Mascheya.Core.Token (Token(lexeme))
import Data.STRef (newSTRef, writeSTRef, readSTRef)
import Control.Monad.ST
import Control.Monad.Trans (lift)

eval :: Expr -> VEnv s -> Out s
eval (VarExpr (Var token)) = maybe error' force . Env.lookup (lexeme token)
  where 
    error' = Result.failT $ RuntimeError $ Result.undefinedVar token
eval (LambdaExpr (Lambda (Var token) body')) = 
  Result.succeedT . FunctionVal . Function (lexeme token) body'
eval (AppExpr (App func arg')) = \env -> eval func env >>= flip handleFuncVal env 
  where 
    handleFuncVal (FunctionVal closure@(Function param' _ funcEnv)) = \callerEnv -> do
      argThunk <- lift $ newSTRef $ Delayed arg' callerEnv
      extendedEnv <- return $ Env.extend param' (Thunk argThunk) funcEnv
      eval (body closure) extendedEnv
    handleFuncVal _ = constFailM $ RuntimeError $ notAFunction func
eval (BuiltinFuncExpr builtin _) = eval' builtin
  where 
    eval' (ArithFunc anyArith) = eval'' anyArith
      where 
        eval'' (Arith Plus a b) = evalArith a b (+) (+)
        eval'' (Arith Minus a b) = evalArith a b (-) (-)
        eval'' (Arith Times a b) = evalArith a b (*) (*)
        eval'' (Arith Divide a b) = evalArith a b div (/)

        evalArith :: Expr -> Expr -> (Int -> Int -> Int) -> 
          (forall a. Fractional a => a -> a -> a) -> VEnv s -> Out s
        evalArith a b f g = runReaderT $ do
          aVal <- ReaderT $ eval a
          bVal <- ReaderT $ eval b
          ReaderT $ case (aVal, bVal) of                    
            ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ newInt $ f i1 i2
            ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> eval $ newFloat $ g f1 f2
            ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> eval $ newDouble $ g d1 d2
            (Bottom, _) -> returnBottom
            (_, Bottom) -> returnBottom
            (_, _) -> constFailM $ InternalError $ TypecheckingFailed                      
    eval' (IfFunc (If cond ifTrue ifFalse)) = \env -> eval cond env >>= flip handle env 
      where 
        handle (ConstVal (BoolVal True)) = eval ifTrue
        handle (ConstVal (BoolVal False)) = eval ifFalse
        handle Bottom = returnBottom
        handle _ = constFailM $ InternalError $ TypecheckingFailed
    eval' (ListFunc Nil) = constSucceedM $ ListVal NilVal
    eval' (ListFunc (Cons h t)) = \env -> do
      dh <- lift $ newSTRef $ Delayed h env
      dt <- lift $ newSTRef $ Delayed t env
      return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
eval (ConstExpr const') = constSucceedM $ ConstVal $ eval' const'
    where 
      eval' (NumConst (CInt int)) = IntVal int
      eval' (NumConst (CFloat float)) = FloatVal float
      eval' (NumConst (CDouble double)) = DoubleVal double
      eval' (CharConst (CChar char)) = CharVal char

force :: Thunk s -> Out s
force (Thunk ref) = lift (readSTRef ref) >>= handleState
  where 
    handleState (Ready out) = return out
    handleState (Delayed expr env) = do
      val <- eval expr env 
      lift $ writeSTRef ref (Ready val)
      return val

reify :: Value s -> ResultT (ST s) PureValue
reify (ThunkVal thunk) = force thunk >>= reify
reify (FunctionVal (Function p b _)) = return $ PureFuncVal $ PureFunction p b
reify (ConstVal c) = return $ PureConstVal c
reify (ListVal NilVal) = return $ PureListVal PureNilVal 
reify (ListVal (ConsVal h t)) = do
  pureH <- reify $ ThunkVal h
  pureT <- reify $ ThunkVal t
  case pureT of
    (PureListVal pureList) -> return $ PureListVal $ PureConsVal pureH pureList
    _ -> Result.failT $ InternalError $ TypecheckingFailed 
reify Bottom = return PureBottom

returnBottom :: VEnv s -> Out s
returnBottom = constSucceedM Bottom

constSucceedM :: Value s -> VEnv s -> Out s
constSucceedM = constM . Result.succeedT

constFailM :: Failure -> VEnv s -> Out s
constFailM = constM . Result.failT

constM :: ResultT (ST s) (Value s) -> VEnv s -> Out s
constM = const