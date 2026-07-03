{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT) )
import Data.STRef (writeSTRef, readSTRef, modifySTRef, STRef)
import Control.Monad.ST
import Control.Monad.Trans (lift)

eval :: CProg -> VEnv s -> Out s
eval (CDefProg def) = evalDef def
eval (CExprProg expr) = evalExpr expr

evalDef :: CDef -> VEnv s -> Out s
evalDef cDef = \env -> do
  envRef <- newLiftedRef $ Env.extend0 env
  def <- register cDef envRef
  return $ DefVal def

evalExpr :: CExpr -> VEnv s -> Out s
evalExpr (CVarExpr (CVar name loc')) = maybe error' force . Env.lookup name
  where 
    error' = Result.failT $ RuntimeError (UndefinedVar name) loc'
evalExpr (CLambdaExpr (CLambda (CVar name _) body')) = 
  Result.succeedT . ClosureVal . Closure name body'
evalExpr (CAppExpr (CApp func arg' source' loc')) = \env -> evalExpr func env >>= flip handleFuncVal env 
  where 
    handleFuncVal (ClosureVal closure@(Closure param' _ funcEnv)) = \callerEnv -> do
      argEnv <- newLiftedRef callerEnv
      
      -- the argument will be evaluated only when needed, using the caller's env
      argState <- newLiftedRef $ Delayed arg' argEnv

      let extendWithParam = Env.extend param' (Thunk argState)
      extendedEnv <- return $ extendWithParam funcEnv

      evalExpr (body closure) extendedEnv
    handleFuncVal _ = constFailT $ RuntimeError (NotAFunction source') loc'
evalExpr (CLetExpr (CLet defs expr)) = \env -> do
  envRef <- newLiftedRef $ Env.extend0 env
  Def letEnvRef <- foldl' 
    (\accDef def -> accDef >>= \(Def accEnv) -> register def accEnv) 
    (Result.succeedT $ Def envRef) defs
  letEnv <- lift $ readSTRef letEnvRef
  evalExpr expr letEnv
evalExpr (CBuiltinFuncExpr builtin) = case builtin of
  CInfixFunc (CInfix a op b) -> case op of
    CArithOp arithOp -> case arithOp of
      CPlus -> evalArith (+)
      CMinus -> evalArith (-)
      CTimes -> evalArith (*)
      CDivide -> evalArithWith div (/) (/)
      CModulo -> evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of 
        ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ newInt $ i1 `mod` i2
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
        aVal <- ReaderT $ evalExpr a
        bVal <- ReaderT $ evalExpr b
        ReaderT $ f aVal bVal

      evalInfix :: (Int -> Int -> CExpr) 
        -> (Float -> Float -> CExpr) 
        -> (Double -> Double -> CExpr)
        -> VEnv s -> Out s
      evalInfix fi ff fd = evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
        ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ fi i1 i2
        ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> evalExpr $ ff f1 f2
        ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> evalExpr $ fd d1 d2
        (Bottom, _) -> returnBottom
        (_, Bottom) -> returnBottom
        (_, _) -> constTypeErrorT

  CIfFunc (CIf cond ifTrue ifFalse) -> \env -> evalExpr cond env >>= flip handle env 
    where 
      handle (ConstVal (BoolVal True)) = evalExpr ifTrue
      handle (ConstVal (BoolVal False)) = evalExpr ifFalse
      handle Bottom = returnBottom
      handle _ = constTypeErrorT
  CListFunc CNil -> constSucceedT $ ListVal NilVal
  CListFunc (CCons h t) -> \envRef -> do
    env <- newLiftedRef envRef
    dh <- newLiftedRef $ Delayed h env
    dt <- newLiftedRef $ Delayed t env
    return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
evalExpr (CConstExpr const') = constSucceedT $ ConstVal $ eval' const'
  where 
    eval' (CNumConst num) = case num of
      CInt int -> IntVal int
      CFloat float -> FloatVal float
      CDouble double -> DoubleVal double
    eval' (CCharConst (CChar char)) = CharVal char
    eval' (CBoolConst CTrue) = BoolVal True
    eval' (CBoolConst CFalse) = BoolVal False

register :: CDef -> STRef s (VEnv s) -> ResultT (ST s) (Def s)
register (CDef lhs' rhs' source loc') = \envRef -> case lhs' of 
  CVarExpr (CVar name _) -> do
    rhsState <- newLiftedRef $ Delayed rhs' envRef
    lift $ modifySTRef envRef (Env.assign name $ Thunk rhsState)
    return $ Def envRef
  CAppExpr CApp { callable, arg } -> case arg of
    CVarExpr cVar -> register (CDef callable (CLambdaExpr $ CLambda cVar rhs') source loc') envRef
    _ -> matchError
  _ -> matchError
  where
    matchError = Result.failT $ RuntimeError (MatchError source) loc'

force :: Thunk s -> Out s
force (Thunk ref) = lift (readSTRef ref) >>= handleState
  where 
    handleState (Ready out) = return out
    handleState (Delayed expr envRef) = do
      env <- lift $ readSTRef envRef
      val <- evalExpr expr env 
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