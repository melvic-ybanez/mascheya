{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mascheya.Core.Result where

import Data.List.NonEmpty
import Mascheya.Core.Types (Pure)
import Mascheya.Core.Display (Display (display))
import Mascheya.Core.Token (Token(Token, line, tokenType, lexeme), TokenType (Eof))

data Failure = LexerError LexerError | ParseError ParseError deriving Show

type Result = Either (NonEmpty Failure) 

succeed :: a -> Result a
succeed = Right 

fail :: Failure -> Result a
fail = Left . singleton

failAll :: NonEmpty Failure -> Result a
failAll = Left

data LexerError = InvalidCharacter { line :: Int, char :: Char } | UnterminatedString { line :: Int }
    deriving Show

data ParseError = Expected { start :: Token, expected :: String, source :: String, at :: String } 
    | InvalidAssignmentTarget Token deriving Show

instance Display Failure where
    display (LexerError error) = display error 
    display (ParseError error) = display error

instance Display LexerError where
    display InvalidCharacter { line, char } = displayLineAndMessage line "Invalid character: " ++ display char
    display UnterminatedString { line } = displayLineAndMessage line "Unterminated string"

instance Display ParseError where
    display Expected { start = Token { line }, expected, source, at } = 
        displayFullLine line source "Expected " ++ source ++ display at ++ "."
    display (InvalidAssignmentTarget Token { line }) = 
        displayLineAndMessage line "Parse Error: Invalid assignment target"

expectedError :: Token -> String -> String -> ParseError
expectedError start expected at = case (tokenType start) of
    Eof -> Expected start expected "at end" at
    _ -> Expected start expected ("at '" ++ display (lexeme start) ++ "'") at

displayLine :: Int -> String
displayLine line = "[line " ++ display line ++ "]"

displayFullLine :: Int -> String -> String -> String
displayFullLine line source message = 
    displayLine line ++ " Error " ++ source ++ ": " ++ message

displayLineAndMessage :: Int -> String -> String
displayLineAndMessage line = displayFullLine line ""