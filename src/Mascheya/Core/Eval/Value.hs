module Mascheya.Core.Eval.Value where

import Mascheya.Core.Result (ResultT)
import Mascheya.Core.Ast.Core (Expr)
import Mascheya.Core.Eval.Env (Env)
import Mascheya.Core.Display (Display(display))
import Data.STRef (STRef)
import Control.Monad.ST
import qualified Mascheya.Core.Lexemes as Lexemes

type VEnv s = Env (Thunk s)

data Value s = ThunkVal (Thunk s) | FunctionVal (Function s) | ConstVal Const | ListVal (List s) | Bottom

newtype Thunk s = Thunk (STRef s (ThunkState s))

data ThunkState s = Delayed Expr (VEnv s) | Ready (Value s)

data Function s = Function { param :: String, body :: Expr, closureEnv :: VEnv s }

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char | BoolVal Bool 

data List s = ConsVal (Thunk s) (Thunk s) | NilVal

type Out s = ResultT (ST s) (Value s)

data PureValue = PureFuncVal PureFunction | PureConstVal Const | PureListVal PureList | PureBottom

data PureFunction = PureFunction String Expr  

data PureList = PureConsVal PureValue PureList | PureNilVal 

instance Display PureValue where
  display (PureFuncVal _) = "<function>"
  display (PureConstVal const') = display const'
  display (PureListVal list) = display list
  display PureBottom = "_|_"

instance Display Const where
  display (IntVal int) = display int
  display (FloatVal float) = display float
  display (DoubleVal double) = display double
  display (CharVal char) = display char
  display (BoolVal bool) = display bool

instance Display PureList where
  display list = display Lexemes.openSquareBracket 
    ++ displayItems list "" ++ display Lexemes.closeSquareBracket
    where 
      displayItems PureNilVal accStr = accStr
      displayItems (PureConsVal h t) accStr = displayItems t $ accStr ++ separator ++ display h
        where
          separator = if null accStr 
            then "" else display Lexemes.comma ++ display Lexemes.space
    
instance Display (Function s) where
  display (Function _ _ _) = "<closure>"