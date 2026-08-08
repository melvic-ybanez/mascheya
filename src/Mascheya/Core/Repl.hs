module Mascheya.Core.Repl where

import System.IO (hFlush, stdout)
import System.Exit (die)
import qualified Mascheya.Core.Eval as Eval
import Mascheya.Core.Display
import Control.Monad.Except (runExceptT)
import Control.Monad.ST (RealWorld)
import Mascheya.Core.Eval (reify)
import qualified Mascheya.Core.Parser as Parser
import Mascheya.Core.Parser (Parser, word, (<&>), matchChar, spaces0, spaces)
import Data.List (stripPrefix, intercalate)
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (lines)
import Data.Char (isSpace)
import Mascheya.Core.Eval.Value (VEnv, Value (DefNelVal), Def (Def))
import Data.STRef (readSTRef)
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Translate as Translate
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Monad.Reader (ReaderT(runReaderT))

data State = State {
  lineMode :: LineMode
  -- TODO: Add more states here
}

data LineMode = Single | Multi deriving Eq

repl :: State -> VEnv RealWorld -> IO ()
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
            result <- runExceptT $ run input'
            newEnv <- handleResult result
            repl state newEnv
      Just rest -> case Parser.parse setArgParser rest of
        Left e -> report $ "Invalid argument pair. " ++ display e
        Right ("line", mode) -> case mode of
          "single" -> repl state { lineMode = Single } env
          "multi" -> repl state { lineMode = Multi } env
          invalid -> report $ "Invalid line mode: " ++ invalid
        Right (arg, _) -> report $ "Invalid argument: " ++ arg
  where 
    run input = do 
      let readerToIO eval = Result.stEitherToIO . runExceptT . runReaderT eval
      sourceProg <- Result.liftT $ Parser.parse Parser.program input
      coreProg <- Result.liftT $ Translate.toCoreProg sourceProg
      val <- readerToIO (Eval.eval coreProg) env
      newEnv <- Result.stEitherToIO $ case val of
        DefNelVal ((Def newEnvRef) :| _) -> do
          newEnv <- readSTRef newEnvRef
          return $ Result.succeed newEnv
        _ -> return $ Result.succeed env 
      reified <- readerToIO (reify val) newEnv
      return (reified, newEnv)
    
    handleResult (Left error') = do
      putErrorLn $ display error'
      return env
    handleResult (Right (val, newEnv)) = do
      putSuccessLn $ display val
      return newEnv
    
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

putInfoLn :: String -> IO ()
putInfoLn = putStrLn . brightCyanStr

putErrorLn :: String -> IO ()
putErrorLn = putStrLn . brightRedStr

putSuccessLn :: String -> IO ()
putSuccessLn = putStrLn . brightGreenStr

-- | Color a string using ANSI escape codes
coloredStr :: Int -> String -> String
coloredStr i msg = "\ESC[" ++ show i ++ "m" ++ msg ++ "\ESC[0m"

brightCyanStr :: String -> String
brightCyanStr = coloredStr 96

brightRedStr :: String -> String
brightRedStr = coloredStr 91

brightGreenStr :: String -> String
brightGreenStr = coloredStr 92