module Mascheya.Core.Result where
import Data.List.NonEmpty

data Failure = Failure

type Result a = Either (NonEmpty Failure) a

succeed :: a -> Result a
succeed = Right 

fail :: Failure -> Result a
fail = Left . singleton

failAll :: NonEmpty Failure -> Result a
failAll = Left