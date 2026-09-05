{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Display (Display (display))
import Prelude hiding (fail, error)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError))
import Control.Monad.Trans (lift)
import Data.IORef (IORef, newIORef)

type Result = Either FailureNel 
type ResultT = ExceptT FailureNel
type FailureNel = NonEmpty Failure

newtype Loc = Loc { line :: Int } deriving (Show, Eq, Ord)

data Failure = ParseError ParseError Loc | RuntimeError RuntimeError Loc
  | InternalError InternalError | TranslationError TranslationError deriving Show

data ParseError = Invalid String String | Expected String | Eof deriving Show

data RuntimeError = UndefinedVar String | NotAFunction String 
  | MatchError | UndefinedCalled | NoIntansce String String
  deriving Show

data InternalError = TypecheckingFailed deriving Show

data TranslationError = UnableToTranslate String Loc deriving Show

succeed :: a -> Result a
succeed = Right 

succeedT :: Monad m => a -> ResultT m a
succeedT = return

liftT :: Monad m => Result a -> ResultT m a
liftT = ExceptT . return

fail :: Failure -> Result a
fail = Left . singleton

failAll :: FailureNel -> Result a
failAll = Left

failT :: Monad m => Failure -> ResultT m a
failT = throwError . singleton

newLiftedRef :: a -> ExceptT FailureNel IO (IORef a)
newLiftedRef = lift . newIORef

parseError :: ParseError -> Loc -> Result a
parseError error = fail . ParseError error

runtimeErrorT :: Monad m => RuntimeError -> Loc -> ResultT m a
runtimeErrorT error = failT . RuntimeError error

undefinedCalled :: Loc -> Failure
undefinedCalled = RuntimeError UndefinedCalled

matchError :: Loc -> Failure
matchError = RuntimeError MatchError

noInstance :: String -> String -> Loc -> Failure
noInstance type' = RuntimeError . NoIntansce type'

formatMsgWithLoc :: String -> Loc -> String
formatMsgWithLoc msg loc = msg ++ " at " ++ display loc ++ ". "

dummyLoc :: Loc
dummyLoc = Loc 1

instance Display Failure where
  display (ParseError error loc) = formatMsgWithLoc "Parser Error" loc ++ display error
  display (RuntimeError error loc) = formatMsgWithLoc "Runtime Error" loc ++ display error
  display (InternalError error) = "Internal Error" ++ " " ++ display error  -- TODO: Format this too
  display (TranslationError (UnableToTranslate error loc)) = formatMsgWithLoc "Translation Error" loc
    ++ error

instance Display ParseError where
  display (Invalid expected source) = "Invalid " ++ expected ++ ": " ++ source
  display (Expected expected) = "Expected: " ++ expected
  display Eof = "End of file"

instance Display RuntimeError where
  display (UndefinedVar name) = "Undefined variable: " ++ name
  display (NotAFunction source) = "Not a function: " ++ source
  display (MatchError) = "Pattern match error."
  display UndefinedCalled = "Undefined called."
  display (NoIntansce type' class') = "No " ++ class' ++ " instance for " ++ type'

instance Display InternalError where
  display TypecheckingFailed = "Typechecker failed to capture a type error"

instance Display Loc where
  display (Loc line') = "line " ++ display line'