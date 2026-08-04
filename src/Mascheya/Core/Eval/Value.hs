module Mascheya.Core.Eval.Value where

import Mascheya.Core.Result (ResultT)
import Mascheya.Core.Eval.Env (Env)
import Mascheya.Core.Display (Display(display), SSV (SSV))
import Data.STRef (STRef)
import Control.Monad.ST
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Ast.Core (CExpr, CPat)
import Data.List.NonEmpty (NonEmpty)

type VEnv s = Env (Thunk s)

data Value s = ThunkVal (Thunk s) 
  | ClosureVal (Closure s) 
  | ConstVal Const 
  | ListVal (List s) 
  | DefNelVal (NonEmpty (Def s))
  | ProdVal (Product s)
  | MatchFailVal
  | MatchErrorVal 
  | BottomVal

newtype Thunk s = Thunk (STRef s (ThunkState s))

data ThunkState s = Delayed CExpr (STRef s (VEnv s)) | Ready (Value s)

data Closure s = Closure { closureParam :: CPat, closureBody :: CExpr, closureEnv :: VEnv s }

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char 
  | BoolVal Bool | Unit deriving Eq

data List s = ConsVal (Thunk s) (Thunk s) | NilVal

newtype Def s = Def (STRef s (VEnv s))

data Product s = Product String [CExpr]

type Out s = ResultT (ST s) (Value s)

data PureValue = PureClosureVal PureClosure | PureConstVal Const 
  | PureListVal PureList | PureDefNelVal | PureConstructorVal PureConstructor
  | PureBottomVal | PureMatchFailVal | PureMatchErrorVal 

data PureClosure = PureClosure CPat CExpr  

data PureList = PureConsVal PureValue PureList | PureNilVal 

data PureConstructor = PureConstructor String [PureValue]

instance Display PureValue where
  display (PureClosureVal _) = "<function>"
  display (PureConstVal const') = display const'
  display (PureListVal list) = display list
  display PureDefNelVal = display Unit
  display (PureConstructorVal (PureConstructor name args)) = name ++ " " ++ (display $ SSV args)
  display PureBottomVal = Lexemes.bottom
  display PureMatchFailVal = "<match-failure>"
  display PureMatchErrorVal = "<match-error>"

instance Display Const where
  display (IntVal int) = display int
  display (FloatVal float) = display float
  display (DoubleVal double) = display double
  display (CharVal char) = display char
  display (BoolVal bool) = display bool
  display Unit = Lexemes.unit

instance Display PureList where
  display list = display Lexemes.openSquareBracket 
    ++ displayItems list "" ++ display Lexemes.closeSquareBracket
    where 
      displayItems PureNilVal accStr = accStr
      displayItems (PureConsVal h t) accStr = displayItems t $ accStr ++ separator ++ display h
        where
          separator = if null accStr 
            then "" else display Lexemes.comma ++ display Lexemes.space
    
instance Display (Closure s) where
  display (Closure _ _ _) = "<closure>"