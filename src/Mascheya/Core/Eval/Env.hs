module Mascheya.Core.Eval.Env where
import Data.Map (Map)
import Mascheya.Core.Eval.Value (Value, Out)
import Mascheya.Core.Ast.Core (Var (Var))
import qualified Data.Map as Map
import Mascheya.Core.Token (Token(lexeme))
import qualified Mascheya.Core.Result as Result
import Mascheya.Core.Result (Failure(RuntimeError), undefinedVar)
import Prelude hiding (lookup)
import Mascheya.Core.Types (Endo)

type Table = Map String Value

data Env = LocalEnv Local | GlobalEnv Global

data Local = Local Table Env
data Global = Global Table

empty :: Env
empty = GlobalEnv $ Global Map.empty

table :: Env -> Table
table (LocalEnv (Local table' _)) = table'
table (GlobalEnv (Global table')) = table'

lookup :: Var -> Env -> Out
lookup var@(Var token) env = maybe lookupOuter Right $ Map.lookup (lexeme token) (table env)
    where lookupOuter = case env of
            (GlobalEnv _) -> Result.fail $ RuntimeError $ undefinedVar token
            (LocalEnv (Local _ enclosing)) -> lookup var enclosing

assign :: String -> Value -> Endo Env
assign varName value (LocalEnv (Local table' enclosing)) = 
    LocalEnv $ Local (Map.insert varName value table') enclosing
assign varName value (GlobalEnv (Global table')) = 
    GlobalEnv $ Global (Map.insert varName value table')