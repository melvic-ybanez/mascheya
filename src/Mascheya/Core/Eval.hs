{-# LANGUAGE RankNTypes #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT) )
import Data.STRef (writeSTRef, readSTRef, modifySTRef)
import Control.Monad.ST
import Control.Monad.Trans (lift)

eval :: CExpr -> VEnv s -> Out s
eval (CVarExpr (CVar name loc')) = maybe error' force . Env.lookup name
  where 
    error' = Result.failT $ RuntimeError (UndefinedVar name) loc'
eval (CLambdaExpr (CLambda (CVar name _) body')) = 
  Result.succeedT . ClosureVal . Closure name body'
eval (CAppExpr (CApp func arg' source' loc')) = \env -> eval func env >>= flip handleFuncVal env 
  where 
    handleFuncVal (ClosureVal closure@(Closure param' _ funcEnv)) = \callerEnv -> do
      argEnv <- newLiftedRef callerEnv
      
      -- the argument will be evaluated only when needed, using the caller's env
      argState <- newLiftedRef $ Delayed arg' argEnv

      let extendWithParam = Env.extend param' (Thunk argState)
      extendedEnv <- return $ extendWithParam funcEnv

      eval (body closure) extendedEnv
    handleFuncVal _ = constFailT $ RuntimeError (NotAFunction source') loc'
eval (CLetExpr (CLet defs expr)) = \env -> do
  Def letEnv <- foldl' 
    (\accDef def -> accDef >>= \(Def accEnv) -> evalCDef def accEnv) 
    (Result.succeedT $ Def env) defs
  eval expr letEnv
eval (CBuiltinFuncExpr builtin) = case builtin of
  CInfixFunc (CInfix a op b) -> case op of
    CArithOp arithOp -> case arithOp of
      CPlus -> evalArith (+)
      CMinus -> evalArith (-)
      CTimes -> evalArith (*)
      CDivide -> evalArithWith div (/) (/)
      CModulo -> evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of 
        ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ newInt $ i1 `mod` i2
        (_, _) -> constTypeErrorT

      where
        evalArith :: (forall a. Num a => a -> a -> a) -> VEnv s -> Out s
        evalArith binOp = evalArithWith binOp binOp binOp

        evalArithWith fi ff fd = evalInfix 
          (\i -> newInt . fi i) (\f -> newFloat . ff f) (\d -> newDouble . fd d)
    
    CCompOp compOp -> case compOp of
      CEqEq -> evalComp (==) 
      CLt -> evalComp (<)
      CLte -> evalComp (<=)
      CGt -> evalComp (>)
      CGte -> evalComp (>=)
      CNotEq -> evalComp (/=)

      where
        evalComp :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> VEnv s -> Out s
        evalComp comp = evalInfix 
          (\i -> newBool . comp i) (\f -> newBool . comp f) (\d -> newBool . comp d)
    where
      evalInfixOpWith f = runReaderT $ do
        aVal <- ReaderT $ eval a
        bVal <- ReaderT $ eval b
        ReaderT $ f aVal bVal

      evalInfix :: (Int -> Int -> CExpr) 
        -> (Float -> Float -> CExpr) 
        -> (Double -> Double -> CExpr)
        -> VEnv s -> Out s
      evalInfix fi ff fd = evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
        ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> eval $ fi i1 i2
        ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> eval $ ff f1 f2
        ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> eval $ fd d1 d2
        (Bottom, _) -> returnBottom
        (_, Bottom) -> returnBottom
        (_, _) -> constTypeErrorT

  CIfFunc (CIf cond ifTrue ifFalse) -> \env -> eval cond env >>= flip handle env 
    where 
      handle (ConstVal (BoolVal True)) = eval ifTrue
      handle (ConstVal (BoolVal False)) = eval ifFalse
      handle Bottom = returnBottom
      handle _ = constTypeErrorT
  CListFunc CNil -> constSucceedT $ ListVal NilVal
  CListFunc (CCons h t) -> \envRef -> do
    env <- newLiftedRef envRef
    dh <- newLiftedRef $ Delayed h env
    dt <- newLiftedRef $ Delayed t env
    return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
eval (CConstExpr const') = constSucceedT $ ConstVal $ eval' const'
  where 
    eval' (CNumConst num) = case num of
      CInt int -> IntVal int
      CFloat float -> FloatVal float
      CDouble double -> DoubleVal double
    eval' (CCharConst (CChar char)) = CharVal char
    eval' (CBoolConst CTrue) = BoolVal True
    eval' (CBoolConst CFalse) = BoolVal False
eval (CDefExpr def) = (DefVal <$>) . evalCDef def 

evalCDef :: CDef -> VEnv s -> ResultT (ST s) (Def s)
evalCDef (CDef lhs' rhs' source loc') = \env -> case lhs' of 
  CVarExpr (CVar name _) -> do
    envRef <- newLiftedRef $ Env.extend0 env
    rhsState <- newLiftedRef $ Delayed rhs' envRef
    lift $ modifySTRef envRef (Env.assign name $ Thunk rhsState)
    newEnv <- lift $ readSTRef envRef
    return $ Def newEnv
  _ -> Result.failT $ RuntimeError (MatchError source) loc'

force :: Thunk s -> Out s
force (Thunk ref) = lift (readSTRef ref) >>= handleState
  where 
    handleState (Ready out) = return out
    handleState (Delayed expr envRef) = do
      env <- lift $ readSTRef envRef
      val <- eval expr env 
      lift $ writeSTRef ref (Ready val)
      return val

reify :: Value s -> ResultT (ST s) PureValue
reify (ThunkVal thunk) = force thunk >>= reify
reify (ClosureVal (Closure p b _)) = return $ PureClosureVal $ PureClosure p b
reify (ConstVal c) = return $ PureConstVal c
reify (ListVal NilVal) = return $ PureListVal PureNilVal 
reify (ListVal (ConsVal h t)) = do
  pureH <- reify $ ThunkVal h
  pureT <- reify $ ThunkVal t
  case pureT of
    (PureListVal pureList) -> return $ PureListVal $ PureConsVal pureH pureList
    _ -> Result.failT $ InternalError $ TypecheckingFailed 
reify (DefVal _) = return PureDef
reify Bottom = return PureBottom

returnBottom :: VEnv s -> Out s
returnBottom = constSucceedT Bottom

constSucceedT :: Value s -> VEnv s -> Out s
constSucceedT = const . Result.succeedT

constFailT :: Failure -> VEnv s -> Out s
constFailT = const . Result.failT

constTypeErrorT :: VEnv s -> Out s
constTypeErrorT = constFailT $ InternalError $ TypecheckingFailed