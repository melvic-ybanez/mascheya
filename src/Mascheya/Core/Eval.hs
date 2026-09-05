{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core 
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader (..) )
import Control.Monad.Trans (lift)
import Data.IORef (IORef, modifyIORef, readIORef, writeIORef)

type EvalOutT a = ReaderT VEnv (ResultT IO) a
type EvalOut = EvalOutT Value

eval :: CProg -> EvalOut
eval (CDefNelProg (CDefNel defNel)) = do
  defNels <- sequence $ evalDef <$> defNel
  return $ DefNelVal defNels
eval (CExprProg expr) = evalExpr expr

evalDef :: CDef -> EvalOutT Def
evalDef cDef = do
  envRef <- ReaderT $ newLiftedRef . Env.extend0
  lift $ register cDef envRef

evalExpr :: CExpr -> EvalOut
evalExpr (CVarExpr var) = evalVar var
evalExpr (CLambdaExpr (CLambda pattern body')) = 
  ReaderT $ Result.succeedT . ClosureVal . Closure pattern body'
evalExpr (CAppExpr (CApp func arg' source' loc')) = do
  callableVal <- evalExpr func
  case callableVal of
    ClosureVal closure -> applyPattern closure arg' source'
    matchFail@(MatchFailVal _) -> liftSucceedT matchFail
    _ -> lift $ Result.runtimeErrorT (NotAFunction source') loc'
evalExpr (CLetExpr (CLet (CDefNel defs) expr)) = do
  envRef <- ReaderT $ newLiftedRef . Env.extend0
  Def letEnvRef <- lift $ foldl' 
    (\accDef def -> accDef >>= \(Def accEnv) -> register def accEnv) 
    (Result.succeedT $ Def envRef) defs
  letEnv <- lift $ lift $ readIORef letEnvRef
  local (const letEnv) $ evalExpr expr
evalExpr (CBuiltinFuncExpr builtin) = case builtin of
  CInfixFunc cInfix -> evalInfix cInfix
  CIfFunc (CIf cond ifTrue ifFalse) -> do
    condVal <- evalExpr cond
    case condVal of
      ConstVal (BoolVal True) -> evalExpr ifTrue
      ConstVal (BoolVal False) -> evalExpr ifFalse
      BottomVal (Bottom at) -> liftBottom at
      _ -> liftTypeErrorT
  CListFunc CNil -> liftSucceedT $ ListVal Nil
  CListFunc (CCons h t) -> do
    env <- ReaderT newLiftedRef 
    dh <- lift $ newLiftedRef $ Delayed h env
    dt <- lift $ newLiftedRef $ Delayed t env
    return $ ListVal $ Cons (Thunk dh) (Thunk dt)
evalExpr (CConstExpr const') = liftSucceedT $ evalConst const'
evalExpr (COrElseExpr (COrElse left right)) = do
  leftVal <- evalExpr left
  case leftVal of
    BottomVal (Bottom at) -> liftBottom at
    (MatchFailVal _) -> evalExpr right
    _ -> liftSucceedT leftVal
evalExpr (CBottomExpr (CBottom at)) = lift $ Result.runtimeErrorT UndefinedCalled at
evalExpr (CProdExpr (CProd name args)) = liftSucceedT $ ProdVal $ Product name args 
evalExpr (CConstructorExpr (CConstructor name loc')) = evalVar $ CVar name loc'
  
evalVar :: CVar -> EvalOut
evalVar (CVar name loc') = ReaderT $ maybe error' force . Env.lookup name
  where 
    error' = Result.runtimeErrorT (UndefinedVar name) loc'

evalConst :: CConst -> Value
evalConst const' = ConstVal $ case const' of
  CNumConst num -> case num of
    CInt int -> IntVal int
    CFloat float -> FloatVal float
    CDouble double -> DoubleVal double
  CCharConst (CChar char) -> CharVal char
  CBoolConst CTrue -> BoolVal True
  CBoolConst CFalse -> BoolVal False
  CUnitConst -> Unit

applyPattern :: Closure -> CExpr -> String -> EvalOut
applyPattern (Closure paramPat body funcEnv) arg' source = do
  case paramPat of 
    CVarPat (CVar paramName _) -> do
      callerEnv <- ask
      argEnv <- lift $ newLiftedRef callerEnv
      
      -- the argument will be evaluated only when needed, using the caller's env
      argState <- lift $ newLiftedRef $ Delayed arg' argEnv

      let extendWithParam = Env.extend paramName (Thunk argState)
      extendedEnv <- return $ extendWithParam funcEnv

      local (const extendedEnv) $ evalExpr body
    CConstPat param' loc' -> do 
      pVal <- evalExpr $ CConstExpr param'
      argVal <- evalExpr arg'
      case (pVal, argVal) of   
        (_, BottomVal (Bottom at)) -> liftBottom at
        (ConstVal pConst, ConstVal aConst) | pConst == aConst -> evalExpr body
        _ -> liftSucceedT $ MatchFailVal $ MatchFail loc'
    CConstructorPat (CConstructor constructorName loc') pats -> do
      argVal <- evalExpr arg'
      case argVal of
        ProdVal (Product argConstructorName argConstructors)
          | constructorName == argConstructorName -> 
              evalExpr (foldl' mkExpr init' $ unit : argConstructors)
          | otherwise -> return $ MatchFailVal $ MatchFail loc'
          where 
            foldedLambda = fromLambdaDetails pats body
            init' = CAppExpr $ CApp foldedLambda unit source loc'
            mkExpr acc expr = CAppExpr $ CApp acc expr source loc'
        _ -> return $ MatchFailVal $ MatchFail loc'

evalInfix :: CInfix -> EvalOut
evalInfix (CInfix a op b) = case op of
  CArithOp arithOp -> case arithOp of
    CPlus -> evalArith (+)
    CMinus -> evalArith (-)
    CTimes -> evalArith (*)
    CDivide -> evalArithWith div (/) (/)
    CModulo -> evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of 
      ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ newInt $ i1 `mod` i2
      (_, _) -> liftTypeErrorT

    where
      evalArith :: (forall a. Num a => a -> a -> a) -> EvalOut 
      evalArith binOp = evalArithWith binOp binOp binOp

      evalArithWith fi ff fd = evalInfix' 
        (\i -> newInt . fi i) (\f -> newFloat . ff f) (\d -> newDouble . fd d)
  
  CCompOp compOp -> case compOp of
    CEqEq -> evalComp (==) 
    CLt -> evalComp (<)
    CLte -> evalComp (<=)
    CGt -> evalComp (>)
    CGte -> evalComp (>=)
    CNotEq -> evalComp (/=)

    where
      evalComp :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> EvalOut 
      evalComp comp = evalInfix' 
        (\i -> newBool . comp i) (\f -> newBool . comp f) (\d -> newBool . comp d)
  where
    evalInfixOpWith f = do
      aVal <- evalExpr a
      bVal <- evalExpr b
      f aVal bVal

    evalInfix' :: (Int -> Int -> CExpr) -> 
      (Float -> Float -> CExpr) -> 
        (Double -> Double -> CExpr) -> 
          EvalOut 
    evalInfix' fi ff fd = evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
      ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ fi i1 i2
      ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> evalExpr $ ff f1 f2
      ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> evalExpr $ fd d1 d2
      (BottomVal (Bottom at), _) -> liftBottom at
      (_, BottomVal (Bottom at)) -> liftBottom at
      (_, _) -> liftTypeErrorT

register :: CDef -> IORef VEnv -> ResultT IO Def
register (CDef (CVar name _) rhs' _) = \envRef -> do
  rhsState <- newLiftedRef $ Delayed rhs' envRef
  lift $ modifyIORef envRef (Env.assign name $ Thunk rhsState)
  return $ Def envRef

force :: Thunk -> Out 
force (Thunk ref) = do
  state <- lift $ readIORef ref
  case state of
    Ready (ThunkVal thunk) -> force thunk
    Ready out -> return out
    Delayed expr envRef -> do
      env <- lift $ readIORef envRef
      val <- runReaderT (evalExpr expr) env
      lift $ writeIORef ref (Ready val)
      return val

liftBottom :: Loc -> EvalOut 
liftBottom = liftSucceedT . BottomVal . Bottom 

liftSucceedT :: Value -> EvalOut
liftSucceedT = lift . Result.succeedT

liftFailT :: Failure -> EvalOutT a
liftFailT = lift . Result.failT

liftTypeErrorT :: EvalOut 
liftTypeErrorT = liftFailT $ InternalError $ TypecheckingFailed