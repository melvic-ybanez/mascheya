module Mascheya.Core.Eval.Env where
import Data.Map (Map)
import Mascheya.Core.Ast.Core (Var (Var))
import qualified Data.Map as Map
import Mascheya.Core.Token (Token(lexeme))
import qualified Mascheya.Core.Result as Result
import Mascheya.Core.Result (Failure(RuntimeError), undefinedVar, Result)
import Prelude hiding (lookup)
import Mascheya.Core.Types (Endo)

type Table a = Map String a

data Env a = LocalEnv (Local a) | GlobalEnv (Global a)

data Local a = Local (Table a) (Env a)
data Global a = Global (Table a)

empty :: Env a
empty = GlobalEnv $ Global Map.empty

table :: Env a -> Table a
table (LocalEnv (Local table' _)) = table'
table (GlobalEnv (Global table')) = table'

lookup :: Var -> Env a -> Result a
lookup var@(Var token) env = maybe lookupOuter Right $ Map.lookup (lexeme token) (table env)
    where lookupOuter = case env of
            (GlobalEnv _) -> Result.fail $ RuntimeError $ undefinedVar token
            (LocalEnv (Local _ enclosing)) -> lookup var enclosing

assign :: String -> a -> Endo (Env a)
assign varName value (LocalEnv (Local table' enclosing)) = 
    LocalEnv $ Local (Map.insert varName value table') enclosing
assign varName value (GlobalEnv (Global table')) = 
    GlobalEnv $ Global (Map.insert varName value table')

extend :: Var -> a -> Endo (Env a)
extend (Var token) val = LocalEnv . Local (Map.singleton (lexeme token) val) 