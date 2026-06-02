module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)
import qualified Mascheya.Core.Lexer as Lexer
import qualified Mascheya.Core.Parser as Parser
import Mascheya.Core.Parser.ParseResult (ParseResult(result))
import qualified Mascheya.Core.Eval as Eval
import Mascheya.Core.Ast.Translation (translateExpr)
import Mascheya.Core.Display
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Except (runExceptT)
import Control.Monad.ST (runST)
import Mascheya.Core.Eval (reify)

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if input == ":q" then die "Bye!"
  else do
    putStrLn $ handleResult $ run input
    repl
  where run input = do 
          tokens <- Lexer.scanTokens input
          sourceExpr <- result $ Parser.parseExpr $ Parser.fromTokens tokens
          coreExpr <- translateExpr sourceExpr
          runST $ runExceptT $ Eval.eval coreExpr Env.empty >>= reify
        handleResult (Left error') = display error'
        handleResult (Right stVal) = display stVal