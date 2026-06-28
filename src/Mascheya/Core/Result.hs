{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Display (Display (display))
import Prelude hiding (fail, error)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.STRef (newSTRef, STRef)
import Control.Monad.ST
import Control.Monad.Trans (lift)

type Result = Either FailureNel 
type ResultT = ExceptT FailureNel 
type FailureNel = NonEmpty Failure

newtype Loc = Loc { line :: Int } deriving (Show, Eq)

data Failure = ParseError ParseError Loc | RuntimeError RuntimeError Loc
  | InternalError InternalError deriving Show

data ParseError = Invalid String String | Expected String | Eof deriving Show

data RuntimeError = UndefinedVar String | NotAFunction String | MatchError String deriving Show

data InternalError = TypecheckingFailed deriving Show

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

newLiftedRef :: a -> ExceptT FailureNel (ST s) (STRef s a)
newLiftedRef = lift . newSTRef

parseError :: ParseError -> Loc -> Result a
parseError error = fail . ParseError error

formatMsgWithLoc :: String -> Loc -> String
formatMsgWithLoc msg loc = msg ++ " at " ++ display loc ++ ". "

dummyLoc :: Loc
dummyLoc = Loc 1

instance Display Failure where
  display (ParseError error loc) = formatMsgWithLoc "Parser Error" loc ++ display error
  display (RuntimeError error loc) = formatMsgWithLoc "Runtime Error" loc ++ display error
  display (InternalError error) = "Internal Error" ++ " " ++ display error  -- TODO: Format this too

instance Display ParseError where
  display (Invalid expected source) = "Invalid " ++ expected ++ ": " ++ source
  display (Expected expected) = "Expected: " ++ expected
  display Eof = "End of file"

instance Display RuntimeError where
  display (UndefinedVar name) = "Undefined variable: " ++ name
  display (NotAFunction source) = "Not a function: " ++ source
  display (MatchError source) = "Pattern match error: " ++ source

instance Display InternalError where
  display TypecheckingFailed = "Typechecker failed to capture a type error"

instance Display Loc where
  display (Loc line') = "line " ++ display line'