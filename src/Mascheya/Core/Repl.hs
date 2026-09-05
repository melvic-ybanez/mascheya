module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)
import Mascheya.Core.Display
import qualified Mascheya.Core.Parser as Parser
import Mascheya.Core.Parser (Parser, word, (<&>), matchChar, spaces0, spaces)
import Data.List (stripPrefix, intercalate)
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (lines)
import Data.Char (isSpace)
import Mascheya.Core.Eval.Value (VEnv)
import Mascheya.Core.Predef
import qualified Mascheya.Core.Runner as Runner

data State = State {
  lineMode :: LineMode
  -- TODO: Add more states here
}

data LineMode = Single | Multi deriving Eq

repl :: State -> VEnv -> IO ()
repl state env = do
  putStr $ brightCyanStr "mascheya> " 
  hFlush stdout

  rawInput <- getLine
  case trim rawInput of
    ":q" -> die $ brightCyanStr "Bye!"
    input -> case stripPrefix ":set" input of
      Nothing -> case (input, lineMode state) of
        ([], Single) -> repl state env
        (_, Single) -> processInput input
        (_, Multi) -> do
          restOfInput <- getMultiLine
          processInput $ input ++ restOfInput
        where 
          processInput input' = do
            newEnv <- Runner.run input' env
            repl state newEnv
      Just rest -> case Parser.parse setArgParser rest of
        Left e -> report $ "Invalid argument pair. " ++ display e
        Right ("line", mode) -> case mode of
          "single" -> repl state { lineMode = Single } env
          "multi" -> repl state { lineMode = Multi } env
          invalid -> report $ "Invalid line mode: " ++ invalid
        Right (arg, _) -> report $ "Invalid argument: " ++ arg
  where 
    report msg = putErrorLn msg >> repl state env

    getMultiLine = recurse []
      where
        recurse lines = do
          rawLine <- getLine
          let line = trim rawLine
          if line == "-- end" then return $ intercalate [Lexemes.newline] $ reverse lines 
          else recurse $ line : lines

    trim = trim' . trim'
      where
        trim' = reverse . dropWhile isSpace

setArgParser :: Parser (String, String)
setArgParser = (\((((_, arg), _), val), _) -> (arg, val)) 
  <$> (spaces <&> word <&> matchChar '=' <&> word <&> spaces0)