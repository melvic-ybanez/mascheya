module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)
import qualified Mascheya.Core.Eval as Eval
import Mascheya.Core.Display
import qualified Mascheya.Core.Eval.Env as Env
import Control.Monad.Except (runExceptT)
import Control.Monad.ST (runST)
import Mascheya.Core.Eval (reify)
import qualified Mascheya.Core.Parser as Parser
import qualified Mascheya.Core.Eval.Builtins as Builtins
import Mascheya.Core.Translate (fullTranslate)
import Mascheya.Core.Parser (Parser, word, (<&>), matchChar, spaces0, spaces)
import Data.List (stripPrefix, intercalate)
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (lines)

data State = State {
  lineMode :: LineMode
}

data LineMode = Single | Multi deriving Eq

repl :: State -> IO ()
repl state = do
  putStr "mascheya > "
  hFlush stdout
  input <- if lineMode state == Single then getLine else getMultiLine
  case input of
    [] -> loop
    ":q" -> die "Bye!"
    _ -> case stripPrefix ":set" input of
      Nothing -> do
        putStrLn $ handleResult $ run input
        loop
      Just rest -> case Parser.parse setArgParser rest of
        Left e -> report $ "Invalid argument pair. " ++ display e
        Right ("line", mode) -> case mode of
          "single" -> repl state { lineMode = Single }
          "multi" -> repl state { lineMode = Multi }
          invalid -> report $ "Invalid line mode: " ++ invalid
        Right (arg, _) -> report $ "Invalid argument: " ++ arg
  where 
    run input = do 
      sourceExpr <- Parser.parse Parser.expr input
      coreExpr <- fullTranslate sourceExpr
      runST $ runExceptT $ do
        env <- Builtins.init Env.empty
        val <- Eval.eval coreExpr env
        reify val
    
    handleResult (Left error') = display error'
    handleResult (Right stVal) = display stVal
    
    report msg = (putStrLn msg) >>= const loop
    loop = repl state
    getMultiLine = recurse []
      where
        recurse lines = do
          line <- getLine
          if line == "-- end" then return $ intercalate [Lexemes.newline] $ reverse lines 
          else recurse $ line : lines

setArgParser :: Parser (String, String)
setArgParser = (\((((_, arg), _), val), _) -> (arg, val)) 
  <$> (spaces <&> word <&> matchChar '=' <&> word <&> spaces0)
