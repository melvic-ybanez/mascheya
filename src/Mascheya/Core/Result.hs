{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Display (Display (display))
import Prelude hiding (fail, error)
import Mascheya.Core.Ast.Core (Expr)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.List (intercalate)

type Result = Either FailureNel 
type ResultT = ExceptT FailureNel 
type FailureNel = NonEmpty Failure

succeed :: a -> Result a
succeed = Right 

succeedT :: Monad m => a -> ResultT m a
succeedT = return

fail :: Failure -> Result a
fail = Left . singleton

failAll :: FailureNel -> Result a
failAll = Left

failT :: Monad m => Failure -> ResultT m a
failT = throwError . singleton

data Failure = ParseError ParseError Int | RuntimeError RuntimeError 
  | InternalError InternalError deriving Show

data ParseError = Invalid String String | Expected String | Eof deriving Show

data RuntimeError = UndefinedVariable String Int | NotAFunction Expr String deriving Show

data InternalError = TypecheckingFailed deriving Show

parseError :: ParseError -> Int -> Result a
parseError error = fail . ParseError error

displayLine :: Int -> String
displayLine line = "[line " ++ display line ++ "]"

displayFullLine :: Int -> String -> String -> String
displayFullLine line source message = 
  displayLine line ++ " Error " ++ source ++ ": " ++ message

displayLineAndMessage :: Int -> String -> String
displayLineAndMessage = flip displayFullLine ""

undefinedVar :: String -> Int -> RuntimeError 
undefinedVar name = UndefinedVariable name 

notAFunction :: Expr -> RuntimeError
notAFunction = flip NotAFunction "Not a function"

tag :: String -> String
tag = (++ "]") . ("[" ++)

instance Display Failure where
  display (ParseError error line) = "Parse Error at line " ++ show line ++ ". " ++ display error
  display (RuntimeError error) = tag "Runtime Error" ++ " " ++ display error
  display (InternalError error) = tag "Internal Error" ++ " " ++ display error

instance Display ParseError where
  display (Invalid expected source) = "Invalid " ++ expected ++ ": " ++ source
  display (Expected expected) = "Expected: " ++ expected
  display Eof = "End of file"

instance Display RuntimeError where
  display (UndefinedVariable name line) = 
    displayLine line ++ ". " ++ "Undefined variable: " ++ name
  display (NotAFunction expr message) = 
    message ++ "\n" ++ display expr ++ ". " 

instance Display InternalError where
  display TypecheckingFailed = "Typechecker failed to capture a type error"