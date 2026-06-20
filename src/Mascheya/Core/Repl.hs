module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)
import qualified Mascheya.Core.Eval as Eval
import Mascheya.Core.Ast.Translation (translateExpr)
import Mascheya.Core.Display
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Except (runExceptT)
import Control.Monad.ST (runST)
import Mascheya.Core.Eval (reify)
import qualified Mascheya.Core.Parser as Parser
import qualified Mascheya.Core.Eval.Builtins as Builtins

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if input == ":q" then die "Bye!"
  else do
    putStrLn $ handleResult $ run input
    repl
  where 
    run input = do 
      sourceExpr <- Parser.parse Parser.expr input
      coreExpr <- translateExpr sourceExpr
      runST $ runExceptT $ do
        env <- Builtins.init Env.empty
        val <- Eval.eval coreExpr env
        reify val
    handleResult (Left error') = display error'
    handleResult (Right stVal) = display stVal