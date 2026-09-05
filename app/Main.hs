module Main where

import Mascheya.Core.Repl
import qualified Mascheya.Core.Predef as Predef
import Control.Monad.Except (runExceptT)
import Control.Monad.ST (stToIO)
import Mascheya.Core.Display (Display(display))
import qualified Mascheya.Core.Eval.Env as Env

main :: IO ()
main = do
  result <- runExceptT $ Predef.init Env.empty
  env <- case result of
    Left err -> (putStrLn $ display err) >> return Env.empty
    Right e -> return e
  repl State { lineMode = Single } env
