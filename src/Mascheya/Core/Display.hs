{-# LANGUAGE DefaultSignatures #-}

module Mascheya.Core.Display where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.List (intercalate)
import qualified Mascheya.Core.Lexemes as Lexemes

class Display a where
  display :: a -> String

  default display :: Show a => a -> String
  display = show

-- space-separated values
newtype SSV a = SSV [a]

instance Display Char
instance Display Int
instance Display Float
instance Display Double
instance Display Bool
instance Display ()

-- TODO: see if we need to wrap NonEmpty in a separate type, like with the SSV above
instance Display a => Display (NonEmpty a) where
  display = intercalate "\n" . fmap display . toList

instance (Display a, Display b) => Display (Either a b) where
  display (Left a) = display a
  display (Right b) = display b

instance Display a => Display (SSV a) where
  display (SSV xs) = intercalate [Lexemes.space] $ fmap display xs

instance (Display a, Display b, Display c) => Display (a, b, c) where
  display (a, b, c) = intercalate [Lexemes.space] [display a, display b, display c]