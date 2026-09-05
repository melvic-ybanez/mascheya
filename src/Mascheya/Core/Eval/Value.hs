{-# OPTIONS_GHC -Wno-orphans #-}

module Mascheya.Core.Eval.Value where

import Mascheya.Core.Result (ResultT, Loc)
import Mascheya.Core.Eval.Env (Env)
import Mascheya.Core.Ast.Core (CExpr, CPat)
import Data.List.NonEmpty (NonEmpty)
import Data.IORef (IORef)

type VEnv = Env Thunk

data Value = ThunkVal Thunk 
  | ClosureVal Closure
  | ConstVal Const 
  | ListVal List
  | DefNelVal (NonEmpty Def)
  | ProdVal Product
  | MatchFailVal MatchFail
  | BottomVal Bottom

newtype Thunk = Thunk (IORef ThunkState) 

data ThunkState = Delayed CExpr (IORef VEnv) | Ready Value 

data Closure = Closure { closureParam :: CPat, closureBody :: CExpr, closureEnv :: VEnv } 

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char 
  | BoolVal Bool | Unit deriving (Eq, Show)

data List = Cons Thunk Thunk | Nil 

newtype Def = Def (IORef VEnv) 

data Product = Product String [CExpr]

data MatchFail = MatchFail Loc

data Bottom = Bottom Loc

type Out = ResultT IO Value
