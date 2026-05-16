module Mascheya.Core.Eval.Value where

data Value = Number Double deriving Show

displayValue :: Value -> String
displayValue (Number value) = show value
    