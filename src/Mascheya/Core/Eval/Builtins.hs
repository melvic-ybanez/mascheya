module Mascheya.Core.Eval.Builtins where
import Mascheya.Core.Eval.Value (VEnv, ThunkState (Delayed), Thunk (Thunk))
import qualified Mascheya.Core.Eval.Env as Env
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Ast.Core 
import Data.STRef (newSTRef)
import Control.Monad.Trans (lift)
import Mascheya.Core.Result (ResultT)
import Control.Monad.ST (ST)

type Builtin s = VEnv s -> ResultT (ST s) (VEnv s)

init :: Builtin s
init = arith  -- TODO: Add more bulitins here

arith :: Builtin s
arith env = arithKind Lexemes.plus CPlus env
  >>= arithKind Lexemes.minus CMinus
  >>= arithKind Lexemes.times CTimes
  >>= arithKind Lexemes.divide CDivide
  >>= arithKind Lexemes.modulo CModulo

arithKind :: Char -> CArithKind -> Builtin s
arithKind lexeme kind env = fmap (\ref -> Env.assign [lexeme] (Thunk ref) env) 
  $ lift $ newSTRef $ Delayed outerClosure env
  where 
    a = newDummyVar "a"
    b = newDummyVar "b"
    outerClosure = CLambdaExpr $ CLambda a innerClosure
    innerClosure = CLambdaExpr $ CLambda b $ CBuiltinFuncExpr 
      $ CArithFunc $ CArith kind (CVarExpr a) (CVarExpr b)