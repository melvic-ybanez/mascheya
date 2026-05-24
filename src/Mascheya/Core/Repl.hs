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
            Eval.eval coreExpr Env.empty
          handleResult (Left error') = display error'
          handleResult (Right value) = display value