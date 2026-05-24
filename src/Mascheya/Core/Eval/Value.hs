module Mascheya.Core.Eval.Value where
import Mascheya.Core.Display (display, Display)
import Mascheya.Core.Result (Result)

data Value = IntValue Int | AppValue Value Value | LambdaValue (Value -> Out)

type Out = Result Value

instance Display Value where
    display (IntValue value) = display value
    display (AppValue callable arg) = display callable ++ " " ++ display arg 
    display (LambdaValue _) = "<lambda>"
    