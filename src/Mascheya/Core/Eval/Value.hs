module Mascheya.Core.Eval.Value where
import Mascheya.Core.Display (display, Display)
import Mascheya.Core.Result (Result)

data Value = IntValue Int | AppValue Value Value deriving Show

type Out = Result Value

instance Display Value where
    display (IntValue value) = display value
    display (AppValue callable arg) = display callable ++ " " ++ display arg 
    