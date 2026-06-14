{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Mascheya.Core.Display where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.List (intercalate)

class Display a where
  display :: a -> String

  default display :: Show a => a -> String
  display = show

instance Display Char
instance Display Int
instance Display Float
instance Display Double
instance Display Bool

instance Display a => Display (NonEmpty a) where
  display = intercalate "\n" . fmap display . toList

instance (Display a, Display b) => Display (Either a b) where
  display (Left a) = display a
  display (Right b) = display b