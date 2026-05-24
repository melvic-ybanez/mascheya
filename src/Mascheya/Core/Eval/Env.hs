module Mascheya.Core.Eval.Env where
import Data.Map (Map)
import Mascheya.Core.Eval.Value (Value, Out)
import Mascheya.Core.Ast.Core (Var (Var))
import qualified Data.Map as Map
import Mascheya.Core.Token (Token(lexeme))
import qualified Mascheya.Core.Result as Result
import Mascheya.Core.Result (Failure(RuntimeError), undefinedVar)
import Mascheya.Core.Types (Endo)

newtype Env = Env (Map String Value)

empty :: Env
empty = Env Map.empty

lookup :: Var -> Env -> Out
lookup (Var token) (Env underlying) = maybe (Result.fail $ RuntimeError $ undefinedVar token) Right 
    $ Map.lookup (lexeme token) underlying

assign :: String -> Value -> Endo Env
assign varName value (Env underlying) = Env $ Map.insert varName value underlying