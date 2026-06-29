{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Mascheya.Core.Parser (
  module Mascheya.Core.Parser.Core,
  module Mascheya.Core.Parser.Primitives,
  module Mascheya.Core.Parser.Prog,
  parse,
) where

import Mascheya.Core.Parser.Core
import Mascheya.Core.Parser.Primitives
import Mascheya.Core.Parser.Prog

import Mascheya.Core.Result (Result, parseError, ParseError (Invalid), Loc (Loc))
import qualified Mascheya.Core.Result as Result

newState :: String -> State
newState = flip State 1

parse :: Parser a -> String -> Result a
parse (Parser run') = (>>= handle) . run' . newState 
  where
    handle (val, (State [] _)) = Result.succeed val
    handle (_, (State rest line')) = parseError (Invalid "characters" rest) (Loc line')
