module Mascheya.Core.Eval.Value where
import Mascheya.Core.Result (Result)
import Mascheya.Core.Ast.Core (Expr, Var)
import Mascheya.Core.Eval.Env (Env)
import Mascheya.Core.Display (Display(display))

data Value = ThunkVal Thunk | ClosureVal Closure | ConstVal Const

data Thunk = Evaluated Value | Delayed Expr (Env Thunk)

data Closure = Closure { param :: Var, body :: Expr, closureEnv :: Env Thunk }

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char | BoolVal Bool

type Out = Result Value

instance Display Value where
    display (ThunkVal thunk) = display thunk
    display (ClosureVal _) = "<closure>"
    display (ConstVal const') = display const'

instance Display Const where
    display (IntVal int) = display int
    display (FloatVal float) = display float
    display (DoubleVal double) = display double
    display (CharVal char) = display char
    display (BoolVal bool) = display bool

instance Display Thunk where
    display (Evaluated value) = display value
    display (Delayed expr _) = "<thunk: " ++ display expr ++ ">"
    
instance Display Closure where
    display (Closure _ _ _) = "<closure>"