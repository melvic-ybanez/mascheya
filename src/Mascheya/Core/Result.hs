module Mascheya.Core.Result where
import Data.List.NonEmpty
import Mascheya.Core.Types (Pure)

data Failure = Failure deriving Show

type Result = Either (NonEmpty Failure) 

succeed :: a -> Result a
succeed = Right 

fail :: Failure -> Result a
fail = Left . singleton

failAll :: NonEmpty Failure -> Result a
failAll = Left
    