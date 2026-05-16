module Mascheya.Core.Eval.Value where
import Mascheya.Core.Display (display, Display)

data Value = Int Int deriving Show

instance Display Value where
    display (Int value) = display value
    