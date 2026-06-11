{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Display (Display (display))
import Prelude hiding (fail, error)
import Mascheya.Core.Ast.Core (Expr)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Mascheya.Core.Token (Token (line))

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

data Failure = ParseError ParseError | RuntimeError RuntimeError 
  | InternalError InternalError deriving Show

data ParseError = Invalid String String | Expected String | Eof deriving Show

data RuntimeError = UndefinedVariable Token String | NotAFunction Expr String deriving Show

data InternalError = TypecheckingFailed deriving Show

parseError :: ParseError -> Result a
parseError = fail . ParseError

displayLine :: Int -> String
displayLine line = "[line " ++ display line ++ "]"

displayFullLine :: Int -> String -> String -> String
displayFullLine line source message = 
  displayLine line ++ " Error " ++ source ++ ": " ++ message

displayLineAndMessage :: Int -> String -> String
displayLineAndMessage = flip displayFullLine ""

undefinedVar :: Token -> RuntimeError 
undefinedVar token = UndefinedVariable token $ "Undefined variable: " ++ display token

notAFunction :: Expr -> RuntimeError
notAFunction = flip NotAFunction "Not a function"

tag :: String -> String
tag = (++ "]") . ("[" ++)

instance Display Failure where
  display (ParseError error) = tag "Parse Error" ++ " " ++ display error
  display (RuntimeError error) = tag "Runtime Error" ++ " " ++ display error
  display (InternalError error) = tag "Internal Error" ++ " " ++ display error

instance Display ParseError where
  display (Invalid expected source) = "Invalid " ++ expected ++ ": " ++ source
  display (Expected expected) = "Expected: " ++ expected
  display Eof = "End of file"

instance Display RuntimeError where
  display (UndefinedVariable token message) = 
    message ++ "\n" ++ displayLine (line token) ++ ". " ++ display token 
  display (NotAFunction expr message) = 
    message ++ "\n" ++ display expr ++ ". " 

instance Display InternalError where
  display TypecheckingFailed = "Typechecker failed to capture a type error"