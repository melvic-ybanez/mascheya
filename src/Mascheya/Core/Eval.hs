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
import Mascheya.Core.Display (Display(display))

eval :: CProg -> VEnv s -> Out s
eval (CDefNelProg (CDefNel defNel)) = \env -> 
  DefNelVal <$> (sequence $ (flip evalDef env) <$> defNel)
eval (CExprProg expr) = evalExpr expr

evalDef :: CDef -> VEnv s -> ResultT (ST s) (Def s)
evalDef cDef = \env -> do
  envRef <- newLiftedRef $ Env.extend0 env
  register cDef envRef

evalExpr :: CExpr -> VEnv s -> Out s
evalExpr (CVarExpr var) = evalVar var
evalExpr (CLambdaExpr (CLambda pattern body')) = 
  Result.succeedT . ClosureVal . Closure pattern body'
evalExpr (CAppExpr (CApp func arg' source' loc')) = \env -> 
  evalExpr func env >>= flip handleFuncVal env 
  where 
    handleFuncVal (ClosureVal closure) = applyPattern closure arg' source'
    handleFuncVal MatchFailVal = constSucceedT MatchFailVal
    handleFuncVal _ = constFailT $ RuntimeError (NotAFunction source') loc'
evalExpr (CLetExpr (CLet defs expr)) = \env -> do
  envRef <- newLiftedRef $ Env.extend0 env
  Def letEnvRef <- foldl' 
    (\accDef def -> accDef >>= \(Def accEnv) -> register def accEnv) 
    (Result.succeedT $ Def envRef) defs
  letEnv <- lift $ readSTRef letEnvRef
  evalExpr expr letEnv
evalExpr (CBuiltinFuncExpr builtin) = case builtin of
  CInfixFunc cInfix -> evalInfix cInfix
  CIfFunc (CIf cond ifTrue ifFalse) -> \env -> evalExpr cond env >>= flip handle env 
    where 
      handle (ConstVal (BoolVal True)) = evalExpr ifTrue
      handle (ConstVal (BoolVal False)) = evalExpr ifFalse
      handle BottomVal = returnBottom
      handle _ = constTypeErrorT
  CListFunc CNil -> constSucceedT $ ListVal NilVal
  CListFunc (CCons h t) -> \envRef -> do
    env <- newLiftedRef envRef
    dh <- newLiftedRef $ Delayed h env
    dt <- newLiftedRef $ Delayed t env
    return $ ListVal $ ConsVal (Thunk dh) (Thunk dt)
evalExpr (CConstExpr const') = constSucceedT $ evalConst const'
evalExpr (COrElseExpr (COrElse left right at)) = runReaderT $ do
  leftVal <- ReaderT $ evalExpr left
  case leftVal of
    MatchFailVal -> do
      rightVal <- ReaderT $ evalExpr right
      case rightVal of
        MatchErrorVal -> ReaderT $ const $ Result.failT $ RuntimeError MatchError at
        _ -> return rightVal
    _ -> return leftVal
evalExpr CBottom = constSucceedT BottomVal
evalExpr (CProdExpr (CProd name args)) = constSucceedT $ ProdVal $ Product name args 
evalExpr (CConstructorExpr (CConstructor name loc')) = evalVar $ CVar name loc'
  
evalVar :: CVar -> VEnv s -> Out s
evalVar (CVar name loc') = maybe error' force . Env.lookup name
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

applyPattern :: Closure s -> CExpr -> String -> VEnv s -> Out s
applyPattern (Closure paramPat body funcEnv) arg' = \source callerEnv -> do
  case paramPat of 
    CVarPat (CVar paramName _) -> do
      argEnv <- newLiftedRef callerEnv
      
      -- the argument will be evaluated only when needed, using the caller's env
      argState <- newLiftedRef $ Delayed arg' argEnv

      let extendWithParam = Env.extend paramName (Thunk argState)
      extendedEnv <- return $ extendWithParam funcEnv

      evalExpr body extendedEnv
    CConstPat _ -> do 
      pVal <- evalExpr arg' callerEnv 
      argVal <- evalExpr arg' callerEnv
      case (pVal, argVal) of   
        (_, BottomVal) -> Result.succeedT BottomVal
        (ConstVal pConst, ConstVal aConst) | pConst == aConst -> evalExpr body callerEnv
        _ -> Result.succeedT MatchFailVal
    CConstructorPat (CConstructor constructorName loc') pats -> do
      argVal <- evalExpr arg' callerEnv
      case argVal of
        ProdVal (Product argConstructorName argConstructors)
          | constructorName == argConstructorName -> 
              evalExpr (foldl' mkExpr init' $ unit : argConstructors) callerEnv
          | otherwise -> return MatchFailVal
          where 
            foldedLambda = fromLambdaDetails pats body
            init' = CAppExpr $ CApp foldedLambda unit source loc'
            mkExpr acc expr = CAppExpr $ CApp acc expr source loc'
        _ -> return MatchFailVal

evalInfix :: CInfix -> VEnv s -> Out s
evalInfix (CInfix a op b) = case op of
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
      evalComp :: (forall a. (Ord a, Eq a) => a -> a -> Bool) -> VEnv s -> Out s
      evalComp comp = evalInfix' 
        (\i -> newBool . comp i) (\f -> newBool . comp f) (\d -> newBool . comp d)
  where
    evalInfixOpWith f = runReaderT $ do
      aVal <- ReaderT $ evalExpr a
      bVal <- ReaderT $ evalExpr b
      ReaderT $ f aVal bVal

    evalInfix' :: (Int -> Int -> CExpr) -> 
      (Float -> Float -> CExpr) -> 
        (Double -> Double -> CExpr) -> 
          VEnv s -> Out s
    evalInfix' fi ff fd = evalInfixOpWith $ \aVal -> \bVal -> case (aVal, bVal) of                    
      ((ConstVal (IntVal i1)), (ConstVal (IntVal i2))) -> evalExpr $ fi i1 i2
      ((ConstVal (FloatVal f1)), (ConstVal (FloatVal f2))) -> evalExpr $ ff f1 f2
      ((ConstVal (DoubleVal d1)), (ConstVal (DoubleVal d2))) -> evalExpr $ fd d1 d2
      (BottomVal, _) -> returnBottom
      (_, BottomVal) -> returnBottom
      (_, _) -> constTypeErrorT

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
      val <- evalExpr expr env 
      lift $ writeSTRef ref (Ready val)
      return val

reify :: Value s -> VEnv s -> ResultT (ST s) PureValue
reify (ThunkVal thunk) env = force thunk >>= flip reify env
reify (ClosureVal (Closure p b _)) _ = return $ PureClosureVal $ PureClosure p b
reify (ConstVal c) _ = return $ PureConstVal c
reify (ListVal NilVal) _ = return $ PureListVal PureNilVal 
reify (ListVal (ConsVal h t)) env = do
  pureH <- reify (ThunkVal h) env
  pureT <- reify (ThunkVal t) env
  case pureT of
    (PureListVal pureList) -> return $ PureListVal $ PureConsVal pureH pureList
    _ -> Result.failT $ InternalError $ TypecheckingFailed 
reify (DefNelVal _) _ = return PureDefNelVal
reify MatchFailVal _ = return PureMatchFailVal
reify MatchErrorVal _ = return PureMatchErrorVal
reify (ProdVal (Product name args)) env = do
  argVals <- sequence $ flip evalExpr env <$> args
  argPureVals <- sequence $ flip reify env <$> argVals
  return $ PureConstructorVal $ PureConstructor name argPureVals
reify BottomVal _ = return PureBottomVal

returnBottom :: VEnv s -> Out s
returnBottom = constSucceedT BottomVal

constSucceedT :: Value s -> VEnv s -> Out s
constSucceedT = const . Result.succeedT

constFailT :: Failure -> VEnv s -> Out s
constFailT = const . Result.failT

constTypeErrorT :: VEnv s -> Out s
constTypeErrorT = constFailT $ InternalError $ TypecheckingFailed