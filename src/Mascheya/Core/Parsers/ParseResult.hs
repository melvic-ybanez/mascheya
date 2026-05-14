module Mascheya.Core.Parsers.ParseResult where

import Mascheya.Core.Result(Result)
import qualified Mascheya.Core.Result as Result
import Mascheya.Core.Parsers.Types
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (map)

data ParseResult a = ParseResult {
    result :: Result a,
    parser :: Parser
}

succeed :: a -> Parser -> ParseResult a
succeed = ParseResult . Result.succeed

fail :: Result.Failure -> Parser -> ParseResult a
fail = ParseResult . Result.fail

failAll :: NonEmpty Result.Failure -> Parser -> ParseResult a
failAll = ParseResult . Result.failAll

fromStep :: Step a -> ParseResult a
fromStep step = succeed (value step) (next step)

map :: (Step a -> Step b) -> ParseResult a -> ParseResult b
map f = fold failAll $ fromStep . f

mapValue :: (a -> b) -> ParseResult a -> ParseResult b
mapValue f parseResult = map (\step -> Step (f (value step)) (parser parseResult)) parseResult

fold :: (NonEmpty Result.Failure -> Parser -> b) -> (Step a -> b) -> ParseResult a -> b 
fold ifError ifSuccess parseResult = handle (result parseResult) (parser parseResult)
    where handle (Left errors) = ifError errors
          handle (Right value) = ifSuccess . (Step value)
