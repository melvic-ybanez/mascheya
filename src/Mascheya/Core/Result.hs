{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Token (Token(Token, line, tokenType, lexeme), TokenType (Eof))
import Prelude hiding (error)
import Mascheya.Core.Ast.Core (Expr)

type Result = Either (NonEmpty Failure) 

succeed :: a -> Result a
succeed = Right 

fail :: Failure -> Result a
fail = Left . singleton

failAll :: NonEmpty Failure -> Result a
failAll = Left

data Failure = LexerError LexerError | ParseError ParseError | RuntimeError RuntimeError deriving Show

data LexerError = InvalidCharacter Int Char | UnterminatedString Int
    deriving Show

data ParseError = Expected ExpectedDetails
    | InvalidAssignmentTarget Token deriving Show

data ExpectedDetails = ExpectedDetails {
    start :: Token, expected :: String, source :: String, at :: String
} deriving Show

data RuntimeError = UndefinedVariable Token String | NotAFunction Expr String deriving Show

instance Display Failure where
    display (LexerError error) = "Lexer Error: " ++ display error 
    display (ParseError error) = "Parse Error: " ++ display error
    display (RuntimeError error) = "Runtime Error: " ++ display error

instance Display LexerError where
    display (InvalidCharacter line char) = displayLineAndMessage line "Invalid character: " ++ display char
    display (UnterminatedString line) = displayLineAndMessage line "Unterminated string"

instance Display ParseError where
    display (Expected (ExpectedDetails (Token { line }) _ source at)) = 
        displayFullLine line source "Expected " ++ source ++ display at ++ "."
    display (InvalidAssignmentTarget Token { line }) = 
        displayLineAndMessage line "Invalid assignment target"

instance Display RuntimeError where
    display = ("Runtime Error: " ++ ) . display' 
        where display' (UndefinedVariable token message) = 
                message ++ "\n" ++ displayLine (line token) ++ ". " ++ display token 
              display' (NotAFunction expr message) = 
                message ++ "\n" ++ display expr ++ ". " 

expectedError :: Token -> String -> String -> ParseError
expectedError start expected at = Expected $ case (tokenType start) of
    Eof -> ExpectedDetails start expected "at end" at
    _ -> ExpectedDetails start expected ("at '" ++ display (lexeme start) ++ "'") at

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