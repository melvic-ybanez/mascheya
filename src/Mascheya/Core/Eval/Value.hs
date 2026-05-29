module Mascheya.Core.Eval.Value where

import Mascheya.Core.Result (Result)
import Mascheya.Core.Ast.Core (Expr)
import Mascheya.Core.Eval.Env (Env)
import Mascheya.Core.Display (Display(display))

data Value = ThunkVal Thunk | FunctionVal Function | ConstVal Const | ListVal List

data Thunk = Thunk Out

data Function = Function { param :: String, body :: Expr, closureEnv :: Env Value }

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char | BoolVal Bool 

data List = ConsVal Expr Expr (Env Value) | NilVal

type Out = Result Value

instance Display Value where
    display (ThunkVal thunk) = display thunk
    display (FunctionVal _) = "<closure>"
    display (ConstVal const') = display const'

    {- TODO: This definition is probably just temporary, because we might have
        to eval the args before printing the list -}
    display (ListVal _) = "<list>" 

instance Display Const where
    display (IntVal int) = display int
    display (FloatVal float) = display float
    display (DoubleVal double) = display double
    display (CharVal char) = display char
    display (BoolVal bool) = display bool

instance Display Thunk where
    display (Thunk result) = display result
    
instance Display Function where
    display (Function _ _ _) = "<closure>"