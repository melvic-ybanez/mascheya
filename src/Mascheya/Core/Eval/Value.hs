module Mascheya.Core.Eval.Value where
import Mascheya.Core.Display (display, Display)
import Mascheya.Core.Result (Result)

data Value = ConstVal Const | AppVal Value Value | LambdaVal (Value -> Out)

data Const = IntVal Int | FloatVal Float | DoubleVal Double | CharVal Char 

type Out = Result Value

instance Display Value where
    display (AppVal callable arg) = display callable ++ " " ++ display arg 
    display (LambdaVal _) = "<lambda>"
    display (ConstVal const') = display const'

instance Display Const where
    display (IntVal int) = display int
    display (FloatVal float) = display float
    display (DoubleVal double) = display double
    display (CharVal char) = display char
    