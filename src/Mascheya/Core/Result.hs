module Mascheya.Core.Result where
import Data.List.NonEmpty

data Failure = Failure

type Result a = Either (NonEmpty Failure) a

succeed :: a -> Result a
succeed = Right 