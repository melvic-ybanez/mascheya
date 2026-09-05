{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Mascheya.Core.Parser.Core where

import Mascheya.Core.Result (Result, ParseError (Eof, Expected), parseError, Loc (Loc))
import Control.Applicative (Alternative (empty, (<|>)))
import qualified Mascheya.Core.Result as Result
import Prelude hiding (repeat, fail)
import Control.Monad (MonadPlus)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|), toList)

data State = State { source :: String, line :: Int }
newtype Parser a = Parser { run :: State -> Result (a, State) }

repeat0 :: Parser a -> Parser [a]
repeat0 p = do
  result <- opt $ repeat p
  return $ case result of
    Nothing -> []
    Just as -> toList as

repeat :: Parser a -> Parser (NonEmpty a)
repeat pa = pa >>= \a -> Parser {
  run = \state -> case run (repeat pa) state of
    Left _ -> Result.succeed (a :| [], state)
    Right (as, rest) -> Result.succeed (a <| as, rest)
}

repSepBy :: Parser d -> Parser a -> Parser (NonEmpty a)
repSepBy pd pa = (\(h, t) -> h :| t) <$> pa <&> repSepBy0 pd pa

repSepBy0 :: Parser d -> Parser a -> Parser [a]
repSepBy0 pd pa = repeat0 $ snd <$> pd <&> pa

fail :: ParseError -> Loc -> Parser a
fail err = Parser . const . parseError err

parseMap :: Read a => (a -> b) -> Parser String -> Parser b
parseMap f = fmap $ f . read

track :: Parser a -> Parser (a, Loc)
track p = p >>= \a -> Parser {
  run = \inp@(State _ line') -> Result.succeed ((a, Loc line'), inp)
}

opt :: Parser a -> Parser (Maybe a)
opt p = Parser {
  run = \state -> Result.succeed $ case run p state of
    Left _ -> (Nothing, state)
    Right (val, newState) -> (Just val, newState) 
}

unit :: Parser a -> Parser ()
unit = ((const ()) <$>)

bracket :: Char -> Parser a -> Char -> Parser a
bracket l p = enclosedLr (matchChar l) p . matchChar

enclosedLr :: Parser l -> Parser a -> Parser r -> Parser a
enclosedLr pl pa pr = (\((_, a), _) -> a) <$> pl <&> pa <&> pr

enclosed :: Parser lr -> Parser a -> Parser a
enclosed plr pa = enclosedLr plr pa plr

matchStr :: String -> Parser String
matchStr src = matchStrExpect src src

matchStrExpect :: String -> String -> Parser String
matchStrExpect [] _ = return []
matchStrExpect (x : xs) err = (\(c, cs) -> c : cs) <$> matchCharExpect x err <&> matchStrExpect xs err

matchChar :: Char -> Parser Char
matchChar c = matchCharExpect c [c]

matchCharExpect :: Char -> String -> Parser Char
matchCharExpect c = satisfyExpect (\a -> a == c) 

chooseChar :: [Char] -> Parser Char
chooseChar cs = chooseCharExpect cs $ "one of " ++ (intercalate ", " $ fmap (: []) cs)

chooseCharExpect :: [Char] -> String -> Parser Char
chooseCharExpect cs = satisfyExpect (`elem` cs) 

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
  empty = fail Eof $ Loc 0
  
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser runA) (Parser runB) = Parser {
    run = \inp -> case runA inp of
      Left errs1 -> case runB inp of
        Left errs2 -> Left $ errs1 <> errs2
        right -> right
      right -> right 
  }

instance MonadPlus Parser where
