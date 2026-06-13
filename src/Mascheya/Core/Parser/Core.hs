{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Mascheya.Core.Parser.Core where

import Mascheya.Core.Result (Result, ParseError (Eof), parseError)
import Control.Applicative (Alternative (empty, (<|>)))
import qualified Mascheya.Core.Result as Result
import Prelude hiding (fail)

data State = State { source :: String, line :: Int }
newtype Parser a = Parser { run :: State -> Result (a, State) }

fail :: ParseError -> Int -> Parser a
fail err = Parser . const . parseError err

parseMap :: Read a => (a -> b) -> Parser String -> Parser b
parseMap f = ((f . read) <$> ) 

withLine :: Parser a -> Parser (a, Int)
withLine p = p >>= \a -> Parser {
  run = \inp@(State _ line') -> Result.succeed ((a, line'), inp)
}

(<&>) :: Parser a -> Parser b -> Parser (a, b)
(<&>) p q = do
  a <- p
  b <- q
  return (a, b)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = p >>= \res -> return $ f res

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ Result.succeed . (v, )

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pab pa = do
    f <- pab
    a <- pa
    return $ f a

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser pa) f = Parser {
    run = \inp -> pa inp >>= \(val, rest) -> run (f val) rest
  }

instance Alternative Parser where  
  empty :: Parser a
  empty = fail Eof 0
  
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser runA) (Parser runB) = Parser {
    run = \inp -> case runA inp of
      Left _ -> runB inp
      right -> right 
  }