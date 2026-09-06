{-# LANGUAGE DefaultSignatures #-}

module Mascheya.Core.Display where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import qualified Mascheya.Core.Lexemes as Lexemes

class Display a where
  display :: a -> String
  default display :: (Show a) => a -> String
  display = show

-- space-separated values
newtype SSV a = SSV [a]

newtype Str = Str String

data Tup a b = a :+: b

instance Display Int

instance Display Float

instance Display Double

instance Display Bool

instance Display ()

instance Display Char where
  display c = [c]

-- TODO: see if we need to wrap NonEmpty in a separate type, like with the SSV above
instance (Display a) => Display (NonEmpty a) where
  display = intercalate "\n" . fmap display . toList

instance (Display a, Display b) => Display (Either a b) where
  display (Left a) = display a
  display (Right b) = display b

instance (Display a) => Display (SSV a) where
  display (SSV xs) = intercalate [Lexemes.space] $ fmap display xs

instance Display Str where
  display (Str string) = string

instance (Display a, Display b) => Display (Tup a b) where
  display (a :+: b) = intercalate [Lexemes.space] [display a, display b]