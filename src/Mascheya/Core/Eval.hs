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
    handleFuncVal _ = constFailT $ RuntimeError $ notAFunction func
eval (BuiltinFuncExpr builtin _) = eval' builtin
  where 
    eval' (ArithFunc anyArith) = eval'' anyArith
      where 
        eval'' (Arith op a b) = case op of
          Plus -> evalArith (+) (+)
          Minus -> evalArith (-) (-)
          Times -> evalArith (*) (*)
          Divide -> evalArith div (/)
          Modulo -> evalArithWith $ \aVal -> \bVal -> case (aVal, bVal) of 
              ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ newInt $ i1 `mod` i2
              (_, _) -> constTypeErrorT

          where 
            evalArithWith f = runReaderT $ do
              aVal <- ReaderT $ eval a
              bVal <- ReaderT $ eval b
              ReaderT $ f aVal bVal

            evalArith :: (Int -> Int -> Int) -> 
              (forall a. Fractional a => a -> a -> a) -> VEnv s -> Out s
            evalArith f g = evalArithWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
                ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ newInt $ f i1 i2
                ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> eval $ newFloat $ g f1 f2
                ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> eval $ newDouble $ g d1 d2
                (Bottom, _) -> returnBottom
                (_, Bottom) -> returnBottom
                (_, _) -> constTypeErrorT
    eval' (IfFunc (If cond ifTrue ifFalse)) = \env -> eval cond env >>= flip handle env 
      where 
        handle (ConstVal (BoolVal True)) = eval ifTrue
        handle (ConstVal (BoolVal False)) = eval ifFalse
        handle Bottom = returnBottom
        handle _ = constTypeErrorT
    eval' (ListFunc Nil) = constSucceedT $ ListVal NilVal
    eval' (ListFunc (Cons h t)) = \env -> do
      dh <- lift $ newSTRef $ Delayed h env
      dt <- lift $ newSTRef $ Delayed t env
      return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
eval (ConstExpr const') = constSucceedT $ ConstVal $ eval' const'
    where 
      eval' (NumConst num) = case num of
        CInt int -> IntVal int
        CFloat float -> FloatVal float
        CDouble double -> DoubleVal double
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
returnBottom = constSucceedT Bottom

constSucceedT :: Value s -> VEnv s -> Out s
constSucceedT = const . Result.succeedT

constFailT :: Failure -> VEnv s -> Out s
constFailT = const . Result.failT

constTypeErrorT :: VEnv s -> Out s
constTypeErrorT = constFailT $ InternalError $ TypecheckingFailed