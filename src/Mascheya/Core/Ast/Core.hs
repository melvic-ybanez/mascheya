{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mascheya.Core.Ast.Core where
import Mascheya.Core.Token (Token (lexeme))

data Expr = VarExpr Var | LambdaExpr Lambda | ConstExpr Const 
    | AppExpr App | BuiltinFuncExpr BuiltinFunc
    deriving Show

newtype Var = Var Token deriving (Show, Eq)
data Const = CInt Int | CChar Char | CFloat Float | CDouble Double deriving Show
data Lambda = Lambda { param :: Var, body :: Expr } deriving Show
data App = App { callable :: Expr, arg :: Expr } deriving Show

data BuiltinFunc = ArithFunc Arith | LogicalFunc Logical | BoolFunc CBool | IfFunc If | ListFunc CList
    deriving Show

data Arith = Plus | Minus | Times | Divide deriving Show
data Logical = And | Or | Not deriving Show
data CBool = CTrue | CFalse deriving Show
data CList = Cons Expr Expr | Head Expr | Tail Expr | Nil deriving Show
data If = If Bool Expr Expr deriving Show

betaReduce :: Lambda -> Expr -> Expr
betaReduce (Lambda param' body') arg' = substitute param' arg' body'

substitute :: Var -> Expr -> Expr -> Expr
substitute var1 expr (VarExpr var2) | var1 == var2 = expr
substitute _ _ varExpr@(VarExpr _) = varExpr
substitute _ _ constExpr@(ConstExpr _) = constExpr
substitute var expr (AppExpr (App func arg')) = 
    AppExpr $ App (substitute var expr func) $ substitute var expr arg'
substitute var expr (LambdaExpr (Lambda param' body')) | var /= param' = 
    substitute var alphaConvertedExpr body'
    where alphaConvertedExpr = if occursFree param' expr 
            then alphaConvert param' (freeVars expr ++ freeVars body') body'
            else expr
substitute _ _ lambdaExpr@(LambdaExpr _) = lambdaExpr
substitute var expr (BuiltinFuncExpr builtin) = BuiltinFuncExpr $ substitute' builtin
    where substitute' (ListFunc (Cons head' tail')) = 
            ListFunc $ Cons (substitute var expr head') (substitute var expr tail')
          substitute' (ListFunc (Head expr1)) = ListFunc $ Head $ substitute var expr expr1
          substitute' (ListFunc (Tail expr1)) = ListFunc $ Tail $ substitute var expr expr1
          substitute' (IfFunc (If cond ifTrue ifFalse)) = 
            IfFunc $ If cond (substitute var expr ifTrue) $ substitute var expr ifFalse
          substitute' expr1 = expr1

{- | Alpha-conversion. Note that this differs from the textbook definition which usually
    goes `\x -> E <-> \y -> E[y/x]`, because here we are relying on a `taken` param which serves as
    an "environment" containing the free variables. This definition, however, is very useful in cases
    where the environment encompasses multiple expressions, as in the definition of `substitute` -}
alphaConvert :: Var -> [Var] -> Expr -> Expr
alphaConvert varToRename@(Var token) taken = convert 
    where convert constExpr@(ConstExpr _) = constExpr
          convert (VarExpr var1) | varToRename == var1 = VarExpr $ newVar
          convert varExpr@(VarExpr _) = varExpr
          convert (AppExpr (App func arg')) = AppExpr $ App (convert func) $ convert arg'
          convert (LambdaExpr (Lambda param' body')) = LambdaExpr $ Lambda newParam $ convert body'
            where newParam = if param' == varToRename then newVar else param'
          convert (BuiltinFuncExpr builtin) = BuiltinFuncExpr $ convert' builtin
            where convert' (ListFunc (Cons head' tail')) = ListFunc $ Cons (convert head') $ convert tail'
                  convert' (ListFunc (Head expr)) = ListFunc $ Head $ convert expr
                  convert' (ListFunc (Tail expr)) = ListFunc $ Tail $ convert expr
                  convert' (IfFunc (If cond ifTrue ifFalse)) = 
                    IfFunc $ If cond (convert ifTrue) $ convert ifFalse
                  convert' expr = expr
          newVar = genUniqueVar taken token

freeVars :: Expr -> [Var]
freeVars = flip freeVars' []
    where freeVars' (ConstExpr _) = id
          freeVars' (VarExpr var) = (var :)
          freeVars' (AppExpr (App func arg')) = \vars -> freeVars' func vars ++ freeVars' arg' vars
          freeVars' (LambdaExpr (Lambda param' body')) = filter (\x -> not $ x == param') . freeVars' body'
          freeVars' (BuiltinFuncExpr (ListFunc (Cons head' tail'))) = 
            \vars -> freeVars' head' vars ++ freeVars' tail' vars 
          freeVars' (BuiltinFuncExpr (ListFunc (Head expr))) = freeVars' expr
          freeVars' (BuiltinFuncExpr (ListFunc (Tail expr))) = freeVars' expr
          freeVars' (BuiltinFuncExpr (IfFunc (If _ ifTrue ifFalse))) = 
            \vars -> freeVars' ifTrue vars ++ freeVars' ifFalse vars
          freeVars' (BuiltinFuncExpr _) = id

genUniqueVar :: [Var] -> Token -> Var
genUniqueVar vars token = gen 0
    where gen counter = if newVar `elem` vars then gen $ counter + 1 else newVar
            where newVar = Var $ token { lexeme = "a" ++ show counter }

{- | Checks if the given variable occurs free in the expression. 
    Note that we are not using the phrase 'is free' because it gives the impression
    that the variable is not bound, which is misleading because a variable can both 
    occur free and occur bound in the same expression. -}
occursFree :: Var -> Expr -> Bool
occursFree x (VarExpr y) = x == y
occursFree _ (ConstExpr _) = False
occursFree x (AppExpr (App f e)) = occursFree x f || occursFree x e
occursFree x (LambdaExpr (Lambda y e)) = 
    -- if x and y are the same, then x is overshadowed and therefore not free
    x /= y && occursFree x e
occursFree x (BuiltinFuncExpr f) = occursInBuiltin f $ occursFree x

{- | Checks if the given variable occurs bound in the expression. 
    Note that we are not using the phrase 'is bound' because it gives the impression
    that the variable is not free, which is misleading because a variable can both 
    occur free and occur bound in the same expression. -}
occursBound :: Var -> Expr -> Bool
occursBound x (AppExpr (App f e)) = occursBound x f || occursBound x e
occursBound x (LambdaExpr (Lambda y e)) = occursBoundOuter || occursBoundInner
    where occursBoundOuter = x == y && occursFree x e
          occursBoundInner = occursBound x e
occursBound x (BuiltinFuncExpr f) = occursInBuiltin f $ occursBound x
occursBound _ _ = False

occursInBuiltin :: BuiltinFunc -> (Expr -> Bool) -> Bool
occursInBuiltin (IfFunc (If _ e1 e2)) occurs = occurs e1 || occurs e2
occursInBuiltin (ListFunc (Cons e1 e2)) occurs = occurs e1 || occurs e2
occursInBuiltin (ListFunc (Head e)) occurs = occurs e
occursInBuiltin (ListFunc (Tail e)) occurs = occurs e
occursInBuiltin _ _ = False

mkVar :: Token -> Expr
mkVar = VarExpr . Var

mkInt :: Int -> Expr
mkInt = ConstExpr . CInt

mkArith :: Arith -> Expr
mkArith = BuiltinFuncExpr . ArithFunc

mkLambda :: Var -> Expr -> Expr
mkLambda param' = LambdaExpr . Lambda param'

mkApp :: Expr -> Expr -> Expr
mkApp callable' = AppExpr . App callable'