{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mascheya.Core.Ast.Core where
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Display (Display(display))
import Mascheya.Core.Token

data Expr = VarExpr Var | LambdaExpr Lambda | ConstExpr Const 
    | AppExpr App | BuiltinFuncExpr BuiltinFunc Int
    deriving Show

newtype Var = Var Token deriving (Show, Eq)

data Const = NumConst AnyNum | CharConst CChar deriving Show

data CIntTag 
data CFloatTag
data CDoubleTag

data Numeric t where
    CInt :: Int -> Numeric CIntTag
    CFloat :: Float -> Numeric CFloatTag
    CDouble ::Double -> Numeric CDoubleTag

data AnyNum = forall t. AnyNum (Numeric t)

data CChar = CChar Char deriving Show

deriving instance Show AnyNum
deriving instance Show (Numeric t)

data Lambda = Lambda { param :: Var, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr } deriving Show

data BuiltinFunc = ArithFunc AnyArith | IfFunc If | ListFunc CList
    deriving Show

data AnyArith = forall a. AnyArith ArithKind (Numeric a) (Numeric a)

deriving instance Show AnyArith

data ArithKind = Plus | Minus | Times | Divide deriving Show
data CList = Cons Expr Expr | Nil deriving Show
data If = If Bool Expr Expr deriving Show

mkVar :: Token -> Expr
mkVar = VarExpr . Var

mkLambda :: Var -> Expr -> Expr
mkLambda param' = LambdaExpr . Lambda param'

mkApp :: Expr -> Expr -> Expr
mkApp callable' = AppExpr . App callable'

mkInt :: Int -> Expr
mkInt = mkNumeric CInt 

mkFloat :: Float -> Expr
mkFloat = mkNumeric CFloat

mkDouble :: Double -> Expr
mkDouble = mkNumeric CDouble

mkNumeric :: (a -> Numeric b) -> a -> Expr
mkNumeric f = ConstExpr . NumConst . AnyNum . f

instance Display Expr where
    display (VarExpr var) = display var
    display (LambdaExpr (Lambda param' body')) = Lexemes.lambdaSymbol ++ display param' ++ " " 
        ++ Lexemes.rightArrow ++ " " ++ display body'
    display (ConstExpr constExpr) = display' constExpr
        where display' (NumConst (AnyNum (CInt int))) = display int
              display' (NumConst (AnyNum (CFloat float))) = display float
              display' (NumConst (AnyNum (CDouble double))) = display double
              display' (CharConst (CChar char')) = display char'
    display (AppExpr (App func arg')) = display func ++ " " ++ display arg'
    display (BuiltinFuncExpr builtin _) = display' builtin
        where display' (ArithFunc (AnyArith Plus _ _)) = Lexemes.plus
              display' (ArithFunc (AnyArith Minus _ _)) = Lexemes.minus
              display' (ArithFunc (AnyArith Times _ _)) = Lexemes.times
              display' (ArithFunc (AnyArith Divide _ _)) = Lexemes.divide
              display' (IfFunc (If cond ifTrue ifFalse)) = Lexemes.ifLexeme ++ " " ++ display cond 
                ++ " " ++ display ifTrue ++ " " ++ display ifFalse
              display' (ListFunc Nil) = Lexemes.openSquareBracket ++ Lexemes.closeSquareBracket
              display' (ListFunc (Cons head' tail')) = display head' ++ " " ++ Lexemes.cons ++ " " ++ display tail'
              
instance Display Var where
    display (Var token) = display token