module Main where

import qualified Mascheya.Core.Eval.Env as Env
import qualified Mascheya.Core.Predef as Predef
import Mascheya.Core.Repl

main :: IO ()
main = do
  env <- Predef.init Env.empty
  repl State {lineMode = Single} env
