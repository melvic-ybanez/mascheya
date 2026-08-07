{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mascheya.Core.Eval where

import Mascheya.Core.Eval.Value 
import Mascheya.Core.Ast.Core hiding (body)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Eval.Env as Env
import Mascheya.Core.Result
import Control.Monad.Reader (ReaderT (ReaderT, runReaderT), MonadReader (..) )
import Data.STRef (writeSTRef, readSTRef, modifySTRef, STRef)
import Control.Monad.ST
import Control.Monad.Trans (lift)

type EvalT s a = ReaderT (VEnv s) (ResultT (ST s)) a
type Eval s = EvalT s (Value s)

eval :: CProg -> Eval s
eval (CDefNelProg (CDefNel defNel)) = do
  defNels <- sequence $ evalDef <$> defNel
  return $ DefNelVal defNels
eval (CExprProg expr) = evalExpr expr

evalDef :: CDef -> EvalT s (Def s)
evalDef cDef = do
  envRef <- ReaderT $ newLiftedRef . Env.extend0
  lift $ register cDef envRef

evalExpr :: CExpr -> Eval s
evalExpr (CVarExpr var) = evalVar var
evalExpr (CLambdaExpr (CLambda pattern body')) = 
  ReaderT $ Result.succeedT . ClosureVal . Closure pattern body'
evalExpr (CAppExpr (CApp func arg' source' loc')) = do
  callableVal <- evalExpr func
  case callableVal of
    ClosureVal closure -> applyPattern closure arg' source'
    MatchFailVal -> liftSucceedT MatchFailVal
    _ -> liftFailT $ RuntimeError (NotAFunction source') loc'
evalExpr (CLetExpr (CLet defs expr)) = do
  envRef <- ReaderT $ newLiftedRef . Env.extend0
  Def letEnvRef <- lift $ foldl' 
    (\accDef def -> accDef >>= \(Def accEnv) -> register def accEnv) 
    (Result.succeedT $ Def envRef) defs
  letEnv <- lift $ lift $ readSTRef letEnvRef
  local (const letEnv) $ evalExpr expr
evalExpr (CBuiltinFuncExpr builtin) = case builtin of
  CInfixFunc cInfix -> evalInfix cInfix
  CIfFunc (CIf cond ifTrue ifFalse) -> do
    condVal <- evalExpr cond
    case condVal of
      ConstVal (BoolVal True) -> evalExpr ifTrue
      ConstVal (BoolVal False) -> evalExpr ifFalse
      BottomVal -> liftBottom
      _ -> liftTypeErrorT
  CListFunc CNil -> liftSucceedT $ ListVal NilVal
  CListFunc (CCons h t) -> do
    env <- ReaderT newLiftedRef 
    dh <- lift $ newLiftedRef $ Delayed h env
    dt <- lift $ newLiftedRef $ Delayed t env
    return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
evalExpr (CConstExpr const') = liftSucceedT $ evalConst const'
evalExpr (COrElseExpr (COrElse left right at)) = do
  leftVal <- evalExpr left
  case leftVal of
    MatchFailVal -> do
      rightVal <- evalExpr right
      case rightVal of
        MatchErrorVal -> ReaderT $ const $ Result.failT $ RuntimeError MatchError at
        _ -> return rightVal
    _ -> return leftVal
evalExpr CBottom = liftSucceedT BottomVal
evalExpr (CProdExpr (CProd name args)) = liftSucceedT $ ProdVal $ Product name args 
evalExpr (CConstructorExpr (CConstructor name loc')) = evalVar $ CVar name loc'
  
evalVar :: CVar -> Eval s
evalVar (CVar name loc') = ReaderT $ maybe error' force . Env.lookup name
  where 
    error' = Result.failT $ RuntimeError (UndefinedVar name) loc'

evalConst :: CConst -> Value s
evalConst const' = ConstVal $ case const' of
  CNumConst num -> case num of
    CInt int -> IntVal int
    CFloat float -> FloatVal float
    CDouble double -> DoubleVal double
  CCharConst (CChar char) -> CharVal char
  CBoolConst CTrue -> BoolVal True
  CBoolConst CFalse -> BoolVal False
  CUnitConst -> Unit

applyPattern :: Closure s -> CExpr -> String -> Eval s
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
    CConstPat param' -> do 
      pVal <- evalExpr $ CConstExpr param'
      argVal <- evalExpr arg'
      case (pVal, argVal) of   
        (_, BottomVal) -> liftSucceedT BottomVal
        (ConstVal pConst, ConstVal aConst) | pConst == aConst -> evalExpr body
        _ -> liftSucceedT MatchFailVal
    CConstructorPat (CConstructor constructorName loc') pats -> do
      argVal <- evalExpr arg'
      case argVal of
        ProdVal (Product argConstructorName argConstructors)
          | constructorName == argConstructorName -> 
              evalExpr (foldl' mkExpr init' $ unit : argConstructors)
          | otherwise -> return MatchFailVal
          where 
            foldedLambda = fromLambdaDetails pats body
            init' = CAppExpr $ CApp foldedLambda unit source loc'
            mkExpr acc expr = CAppExpr $ CApp acc expr source loc'
        _ -> return MatchFailVal

evalInfix :: CInfix -> Eval s
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
      evalArith :: (forall a. Num a => a -> a -> a) -> Eval s
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
      evalComp :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> Eval s
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
          Eval s
    evalInfix' fi ff fd = evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
      ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ fi i1 i2
      ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> evalExpr $ ff f1 f2
      ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> evalExpr $ fd d1 d2
      (BottomVal, _) -> liftBottom
      (_, BottomVal) -> liftBottom
      (_, _) -> liftTypeErrorT

register :: CDef -> STRef s (VEnv s) -> ResultT (ST s) (Def s)
register (CDef (CVar name _) rhs' _) = \envRef -> do
  rhsState <- newLiftedRef $ Delayed rhs' envRef
  lift $ modifySTRef envRef (Env.assign name $ Thunk rhsState)
  return $ Def envRef

force :: Thunk s -> Out s
force (Thunk ref) = lift (readSTRef ref) >>= handleState
  where 
    handleState (Ready out) = return out
    handleState (Delayed expr envRef) = do
      env <- lift $ readSTRef envRef
      val <- runReaderT (evalExpr expr) env
      lift $ writeSTRef ref (Ready val)
      return val

reify :: Value s -> EvalT s PureValue
reify (ThunkVal thunk) = lift (force thunk) >>= reify
reify (ClosureVal (Closure p b _)) = return $ PureClosureVal $ PureClosure p b
reify (ConstVal c) = return $ PureConstVal c
reify (ListVal NilVal) = return $ PureListVal PureNilVal 
reify (ListVal (ConsVal h t)) = do
  pureH <- reify (ThunkVal h) 
  pureT <- reify (ThunkVal t) 
  case pureT of
    (PureListVal pureList) -> return $ PureListVal $ PureConsVal pureH pureList
    _ -> liftFailT $ InternalError $ TypecheckingFailed 
reify (DefNelVal _) = return PureDefNelVal
reify MatchFailVal = return PureMatchFailVal
reify MatchErrorVal = return PureMatchErrorVal
reify (ProdVal (Product name args)) = do
  argVals <- sequence $ evalExpr <$> args
  argPureVals <- sequence $ reify <$> argVals
  return $ PureConstructorVal $ PureConstructor name argPureVals
reify BottomVal = return PureBottomVal

liftBottom :: Eval s
liftBottom = liftSucceedT BottomVal

liftSucceedT :: Value s -> Eval s
liftSucceedT = lift . Result.succeedT

liftFailT :: Failure -> EvalT s a
liftFailT = lift . Result.failT

liftTypeErrorT :: Eval s
liftTypeErrorT = liftFailT $ InternalError $ TypecheckingFailed