{-# LANGUAGE NamedFieldPuns #-}

module Mascheya.Core.Lexer where

import Mascheya.Core.Result (
        Result, 
        succeed, 
        LexerError (InvalidCharacter), 
        Failure (LexerError)
    )
import Mascheya.Core.Token (Token(Token), fromLine, TokenType)
import qualified Data.Char as Char
import Mascheya.Core.Types
import Data.Char(chr)
import qualified Mascheya.Core.Token as Token
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Lexemes as Lexemes
import qualified Mascheya.Core.Token as Token

data Lexer = Lexer {
  source :: String,
  start :: Int,
  current :: Int,
  line :: Int,
  tokenStack :: [Token]
}

type Scan = Pure Result Lexer

lexeme :: Lexer -> String
lexeme Lexer { start, current, source } = take (current - start) $ drop start source

fromSource :: String -> Result Lexer
fromSource source = fmap add1Line $ loop initLexer
  where 
    initLexer = Lexer source 0 0 1 []

    loop lexer | isAtEnd lexer = succeed lexer
    loop lexer = handle $ scanNext lexer
      where handle error'@(Left _) = error'
            handle (Right lexer') = loop lexer'

    add1Line lexer = updateTokens (\tokens' -> (fromLine $ line lexer) : tokens') lexer 

tokens :: Lexer -> [Token]
tokens = reverse . tokenStack

scanTokens :: String -> Result [Token]
scanTokens source = fmap tokens $ scanNext initLexer
  where initLexer = Lexer source 0 0 1 []

scanNext :: Scan
scanNext = scan . prepareNext
  where prepareNext lexer = lexer { start = current lexer }

scan :: Scan
scan lexer = case char of 
  c | isDigit c -> succeed $ scanNumber updatedLexer
  c -> Result.fail $ LexerError $ InvalidCharacter (line lexer) c 
  where (char, updatedLexer) = readAndAdvance lexer

readAndAdvance :: Lexer -> (Char, Lexer)
readAndAdvance lexer = (source lexer !! current lexer, advance lexer)

advance :: Endo Lexer
advance lexer = lexer { current = current lexer + 1 }

scanNumber :: Endo Lexer
scanNumber lexer = addToken (newTokenF fractional) fractional
  where
    newIntToken = newToken $ Token.TInt . read 
    newFloatToken = newToken $ Token.TFloat . read  
    newToken f = Token.LiteralType . f . lexeme
    
    wholeNumber = scanWholeNumber lexer
    (fractional, newTokenF) = if (peek wholeNumber) == Lexemes.dot && (isDigit $ peekNext wholeNumber)
      then (scanWholeNumber . advance $ wholeNumber, newFloatToken) else (wholeNumber, newIntToken)

scanWholeNumber :: Endo Lexer
scanWholeNumber = advanceWhile $ isDigit . peek

advanceWhile :: (Lexer -> Bool) -> Endo Lexer
advanceWhile pred' lexer = if pred' lexer then advance lexer else lexer

peek :: Lexer -> Char
peek = peekN 1

peekNext :: Lexer -> Char
peekNext = peekN 2

peekN :: Int -> Lexer -> Char
peekN n Lexer { source, current } = if index >= (length source) 
  then chr 0 else source !! index
  where index = current + n - 1

{- | Checks if the character is a digit. 
  We are abstracting over the built-in `Char.isDigit` -}
isDigit :: Char -> Bool
isDigit = Char.isDigit

isAtEnd :: Lexer -> Bool
isAtEnd lexer = current lexer >= (length $ source lexer)

updateTokens :: Endo [Token] -> Endo Lexer
updateTokens f lexer = lexer { tokenStack = f (tokenStack lexer) }

addToken :: TokenType -> Endo Lexer
addToken tokenType lexer = updateTokens add' lexer
  where add' tokens' = (Token tokenType (lexeme lexer) (line lexer)) : tokens'