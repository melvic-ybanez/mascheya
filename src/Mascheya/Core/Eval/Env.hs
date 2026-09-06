module Mascheya.Core.Eval.Env where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Mascheya.Core.Types (Endo)
import Prelude hiding (lookup)

type Table a = Map String a

data Env a = LocalEnv (Local a) | GlobalEnv (Global a) deriving (Show)

data Local a = Local (Table a) (Env a) deriving (Show)

data Global a = Global (Table a) deriving (Show)

empty :: Env a
empty = GlobalEnv $ Global Map.empty

table :: Env a -> Table a
table (LocalEnv (Local table' _)) = table'
table (GlobalEnv (Global table')) = table'

lookup :: String -> Env a -> Maybe a
lookup name env = Map.lookup name (table env) <|> lookupOuter
  where
    lookupOuter = case env of
      (GlobalEnv _) -> Nothing
      (LocalEnv (Local _ enclosing)) -> lookup name enclosing

assign :: String -> a -> Endo (Env a)
assign varName value (LocalEnv (Local table' enclosing)) =
  LocalEnv $ Local (Map.insert varName value table') enclosing
assign varName value (GlobalEnv (Global table')) =
  GlobalEnv $ Global (Map.insert varName value table')

extend :: String -> a -> Endo (Env a)
extend name val = LocalEnv . Local (Map.singleton name val)

extend0 :: Endo (Env a)
extend0 = LocalEnv . Local Map.empty