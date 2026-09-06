module Main where

import Control.Monad.Except (runExceptT)
import Mascheya.Core.Display (Display (display))
import qualified Mascheya.Core.Eval.Env as Env
import qualified Mascheya.Core.Predef as Predef
import Mascheya.Core.Repl

main :: IO ()
main = do
  result <- runExceptT $ Predef.init Env.empty
  env <- case result of
    Left err -> (putStrLn $ display err) >> return Env.empty
    Right e -> return e
  repl State {lineMode = Single} env
