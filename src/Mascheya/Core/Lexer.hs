module Mascheya.Core.Lexer(scan) where

import Mascheya.Core.Result (Result, succeed)
import Mascheya.Core.Token (Token)
import qualified Data.Char as Char
import Mascheya.Core.Types
import Data.Char(chr)

data Lexer = Lexer {
    source :: String,
    start :: Int,
    current :: Int,
    line :: Int,
    tokens :: [Token]
}

scan :: Lexer -> Result Lexer
scan lexer = case char of 
    c | isDigit c -> succeed $ scanInt updatedLexer
    where (char, updatedLexer) = readAndAdvance lexer

readAndAdvance :: Lexer -> (Char, Lexer)
readAndAdvance lexer = (source lexer !! current lexer, advance lexer)

advance :: Endo Lexer
advance lexer = lexer { current = current lexer + 1 }

scanInt :: Endo Lexer
scanInt = advanceWhile $ \lexer -> isDigit $ peek lexer

advanceWhile :: (Lexer -> Bool) -> Endo Lexer
advanceWhile pred lexer | not $ pred lexer = lexer
advanceWhile _ lexer = advance lexer

peek :: Lexer -> Char
peek = peekN 1

peekN :: Int -> Lexer -> Char
peekN n lexer = if index >= (length $ source lexer) 
    then chr 0 else (source lexer) !! index
    where index = current lexer + n - 1

-- | Checks if the character is a digit. 
-- 
-- We are abstracting over the built-in `Char.isDigit`
isDigit :: Char -> Bool
isDigit = Char.isDigit