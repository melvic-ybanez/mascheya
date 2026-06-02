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
      where handle error@(Left _) = error
            handle (Right lexer) = loop lexer

    add1Line lexer = updateTokens (\tokens -> (fromLine $ line lexer) : tokens) lexer 

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
  c | isDigit c -> succeed $ scanInt updatedLexer
  c -> Result.fail $ LexerError $ InvalidCharacter (line lexer) c 
  where (char, updatedLexer) = readAndAdvance lexer

readAndAdvance :: Lexer -> (Char, Lexer)
readAndAdvance lexer = (source lexer !! current lexer, advance lexer)

advance :: Endo Lexer
advance lexer = lexer { current = current lexer + 1 }

scanInt :: Endo Lexer
scanInt lexer = addToken (newToken wholeNumber) wholeNumber
  where 
    scanWholeNumber = advanceWhile $ isDigit . peek
    newToken = Token.Literal . Token.Int . read . lexeme
    wholeNumber = scanWholeNumber lexer

advanceWhile :: (Lexer -> Bool) -> Endo Lexer
advanceWhile pred lexer | not $ pred lexer = lexer
advanceWhile _ lexer = advance lexer

peek :: Lexer -> Char
peek = peekN 1

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

updateTokens :: ([Token] -> [Token]) -> Endo Lexer
updateTokens f lexer = lexer { tokenStack = f (tokenStack lexer) }

addToken :: TokenType -> Endo Lexer
addToken tokenType lexer = updateTokens addToken lexer
  where addToken tokens = (Token tokenType (lexeme lexer) (line lexer)) : tokens