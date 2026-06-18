{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Mascheya.Core.Parser.Core where

import Mascheya.Core.Result (Result, ParseError (Eof, Expected), parseError, Loc (Loc))
import Control.Applicative (Alternative (empty, (<|>)))
import qualified Mascheya.Core.Result as Result
import Prelude hiding (repeat, fail)
import Control.Monad (MonadPlus)

data State = State { source :: String, line :: Int }
newtype Parser a = Parser { run :: State -> Result (a, State) }

repeat :: Parser a -> Parser [a]
repeat pa = pa >>= \a -> Parser {
  run = \state -> case run (repeat pa) state of
    Left _ -> Result.succeed ([a], state)
    Right (as, rest) -> Result.succeed (a : as, rest)
}

fail :: ParseError -> Int -> Parser a
fail err = Parser . const . parseError err . Loc

parseMap :: Read a => (a -> b) -> Parser String -> Parser b
parseMap f = fmap (f . read)

track :: Parser a -> Parser (a, Int)
track p = p >>= \a -> Parser {
  run = \inp@(State _ line') -> Result.succeed ((a, line'), inp)
}

opt :: Parser a -> Parser (Maybe a)
opt p = Parser {
  run = \state -> Result.succeed $ case run p state of
    Left _ -> (Nothing, state)
    Right (val, newState) -> (Just val, newState) 
}

bracket :: Char -> Parser a -> Char -> Parser a
bracket l p r = (\((_, a), _) -> a) <$> char l <&> p <&> char r

str :: String -> Parser String
str src = strExpect src src

strExpect :: String -> String -> Parser String
strExpect [] _ = return []
strExpect (x : xs) err = (\(c, cs) -> c : cs) <$> charExpect x err <&> strExpect xs err

char :: Char -> Parser Char
char c = charExpect c [c]

charExpect :: Char -> String -> Parser Char
charExpect c = satisfyExpect (\a -> a == c) 

item :: Parser Char
item = Parser $ \(State inp line') -> case inp of
  [] -> parseError Eof $ Loc line'
  (x : xs) -> Result.succeed (x, State xs (if x == '\n' || x == '\r' then line' + 1 else line'))

satisfyExpect :: (Char -> Bool) -> String -> Parser Char
satisfyExpect p e = satisfy p $ Expected e

satisfy :: (Char -> Bool) -> ParseError -> Parser Char
satisfy p e = track item >>= \(x, l) -> if p x then return x else fail e l

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
      Left errs1 -> case runB inp of
        Left errs2 -> Left $ errs1 <> errs2
        right -> right
      right -> right 
  }

instance MonadPlus Parser where
