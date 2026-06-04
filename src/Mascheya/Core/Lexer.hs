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
      | otherwise = handle $ scanNext lexer
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
scan lexer = scanWith $ case char of 
  c | isDigit c -> succeed . scanNumber
    | c == Lexemes.leftParen -> addTokenOk Token.LeftParen 
    | c == Lexemes.rightParen -> addTokenOk Token.RightParen
    | c == Lexemes.lambdaSymbol -> addTokenOk Token.LambdaSymbol
    | c == Lexemes.equals -> addTokenOrElse c Token.Equals Token.DoubleEquals    
    | otherwise -> const $ Result.fail $ LexerError $ InvalidCharacter (line lexer) c 
  where 
    scanWith = ( $ advancedLexer)
    (char, advancedLexer) = readAndAdvance lexer

readAndAdvance :: Lexer -> (Char, Lexer)
readAndAdvance lexer = (source lexer !! current lexer, advance lexer)

advance :: Endo Lexer
advance lexer = lexer { current = current lexer + 1 }

scanNumber :: Endo Lexer
scanNumber lexer = addToken (newTokenF number) number
  where
    newIntToken = newToken $ Token.TInt . read 
    newFloatToken = newToken $ Token.TFloat . read  
    newDoubleToken = newToken $ Token.TDouble . read
    newToken f = Token.LiteralType . f . lexeme
    
    wholeNumber = scanWholeNumber lexer
    (number, newTokenF) = 
      if peek wholeNumber == Lexemes.dot && (isDigit $ peekNext wholeNumber)
      then 
        let fractional = scanWholeNumber . advance $ wholeNumber
            newFracToken = if peek fractional == Lexemes.floatSuffix 
              then newFloatToken else newDoubleToken
        in (fractional, newFracToken)
      else (wholeNumber, newIntToken)

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

addTokenOk :: TokenType -> Scan
addTokenOk tokenType = Result.succeed . addToken tokenType

addToken :: TokenType -> Endo Lexer
addToken tokenType lexer = updateTokens add' lexer
  where add' tokens' = (Token tokenType (lexeme lexer) (line lexer)) : tokens'

addTokenOrElse :: Char -> TokenType -> TokenType -> Scan
addTokenOrElse char typeIfNotMatched typeIfMatched lexer = Result.succeed 
  $ maybe (addToken typeIfMatched lexer) (addToken typeIfNotMatched) 
  $ matchChar char lexer

matchChar :: Char -> Pure Maybe Lexer
matchChar expected lexer = 
  if isAtEnd lexer || source lexer !! current lexer /= expected 
  then Nothing else Just $ advance lexer  