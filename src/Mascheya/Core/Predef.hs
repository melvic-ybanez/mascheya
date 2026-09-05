module Mascheya.Core.Predef (
  module Mascheya.Core.Predef.Printer,
  init
) where

import Mascheya.Core.Predef.Printer

import Mascheya.Core.Eval.Value (VEnv, ThunkState (Delayed), Thunk (Thunk))
import qualified Mascheya.Core.Eval.Env as Env
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Ast.Core 
import Mascheya.Core.Result (ResultT, newLiftedRef)

import Prelude hiding (init)

type Builtin = VEnv -> ResultT IO VEnv

init :: Builtin 
init env = initArith env >>= initComp 

initArith :: Builtin 
initArith env = arithKind Lexemes.plus CPlus env
  >>= arithKind Lexemes.minus CMinus
  >>= arithKind Lexemes.times CTimes
  >>= arithKind Lexemes.divide CDivide
  >>= arithKind Lexemes.modulo CModulo
  where 
    arithKind lexeme kind = initInfix [lexeme] (CArithOp kind)

initComp :: Builtin 
initComp env = compKind Lexemes.equalsEquals CEqEq env
  >>= compKind Lexemes.notEquals CNotEq
  >>= compKind [Lexemes.lessThan] CLt
  >>= compKind Lexemes.lessThanEquals CLte
  >>= compKind [Lexemes.greaterThan] CGt
  >>= compKind Lexemes.greaterThanEquals CGte
  where
    compKind lexeme kind = initInfix lexeme (CCompOp kind)

initInfix :: String -> CInfixOp -> Builtin
initInfix opLexeme infixOp env = fmap (\ref -> Env.assign opLexeme (Thunk ref) env) 
  $ newLiftedRef env >>= newLiftedRef . Delayed outerClosure
  where 
    a = newDummyVar "a"
    b = newDummyVar "b"
    outerClosure = CLambdaExpr $ CLambda (CVarPat a) innerClosure
    innerClosure = CLambdaExpr $ CLambda (CVarPat b) $ CBuiltinFuncExpr 
      $ CInfixFunc $ CInfix (CVarExpr a) infixOp (CVarExpr b)