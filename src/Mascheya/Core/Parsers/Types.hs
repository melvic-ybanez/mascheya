module Mascheya.Core.Parsers.Types where
import Mascheya.Core.Token

data Parser = Parser {
    tokens :: [Token],
    current :: Int
}

data Step a = Step {
    value :: a,
    next :: Parser
}