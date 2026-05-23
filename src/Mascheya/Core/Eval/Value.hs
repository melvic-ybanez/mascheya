module Mascheya.Core.Eval.Value where
import Mascheya.Core.Display (display, Display)
import Mascheya.Core.Result (Result)

data Value = Int Int deriving Show

type Out = Result Value

instance Display Value where
    display (Int value) = display value
    