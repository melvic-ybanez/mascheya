module Mascheya.Core.Parser.ExprParser where

import Mascheya.Core.Ast.Source 
import Control.Applicative ((<|>))
import Mascheya.Core.Parser.Core (Parser, parseMap, (<&>), repeat, track, repeat0, matchChar, matchStr)
import Mascheya.Core.Parser.Primitives 
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (repeat)
import Mascheya.Core.Result (Loc)
import Data.List.NonEmpty (fromList)

expr :: Parser SExpr
expr = expr' <|> inParens expr'
  where 
    expr' = compExpr

appExpr :: Parser SExpr
appExpr = toExpr <$> (track $ factor' <&> args')
  where 
    factor' = factor <|> (inParens $ appExpr)
    args' = repeat0 arg
    arg = snd <$> matchChar Lexemes.space <&> factor'

    toExpr ((f, []), _) = f
    toExpr ((f, as), at) = SAppExpr $ SApp f (fromList as) at 

compExpr :: Parser SExpr
compExpr = toInfix $ factor' <&> restA
  where
    restA = repeat0 $ inSpaces comparison <&> factor'
    factor' = arithExpr <|> (inParens compExpr)

arithExpr :: Parser SExpr
arithExpr = toInfix $ term <&> restT
  where
    restT = repeat0 $ inSpaces plusOrMinusExpr <&> term 
    term = toInfix $ factor' <&> restF
    restF = repeat0 $ inSpaces termOpExpr <&> factor'
    factor' = appExpr <|> (inParens arithExpr) 

factor :: Parser SExpr
factor = factor' <|> inParens factor'
  where 
    factor' = variable <|> literal
    literal = SLitExpr <$> lit
    variable = SVarExpr <$> var

var :: Parser SVar
var = (uncurry SVar) <$> functionId

lit :: Parser SLit
lit = numLit <|> charLit <|> boolLit 
  where
    numLit = SNumLit <$> num
    charLit = SCharLit <$> char
    boolLit = SBoolLit <$> bool

num :: Parser SNum
num = floatLit <|> doubleLit <|> intLit
  where
    intLit = SIntNum <$> int
    doubleLit = SDoubleNum <$> double
    floatLit = SFloatNum <$> float

int :: Parser SInt
int = parseMap SInt rawInt

double :: Parser SDouble
double = parseMap SDouble rawDouble

float :: Parser SFloat
float = parseMap SFloat rawFloat

char :: Parser SChar
char = (\((_, content), _) -> SChar content) <$> singleQuote 
  <&> (escaped <|> unescaped) <&> singleQuote

bool :: Parser SBool
bool = sTrue <|> sFalse
  where
    sTrue = (const STrue) <$> true
    sFalse = (const SFalse) <$> false

sVar :: String -> Parser SVar
sVar = ((uncurry SVar) <$>) . track . matchStr

comparison :: Parser SVar
comparison = eqEq <|> notEq <|> lessThanEq <|> greaterThanEq <|> lessThan<|> greaterThan

eqEq :: Parser SVar
eqEq = sVar Lexemes.equalsEquals

notEq :: Parser SVar
notEq = sVar Lexemes.notEquals

lessThan :: Parser SVar
lessThan = sVar [Lexemes.lessThan]

greaterThan :: Parser SVar
greaterThan = sVar [Lexemes.greaterThan]

lessThanEq :: Parser SVar
lessThanEq = sVar Lexemes.lessThanEquals

greaterThanEq :: Parser SVar
greaterThanEq = sVar Lexemes.greaterThanEquals

plusOrMinusExpr :: Parser SVar
plusOrMinusExpr = trackedCharToVar <$> track plusOrMinus

termOpExpr :: Parser SVar
termOpExpr = trackedCharToVar <$> track termOp

trackedCharToVar :: (Char, Loc) -> SVar
trackedCharToVar (c, l) = SVar [c] l

toInfix :: Parser (SExpr, [(SVar, SExpr)]) -> Parser SExpr
toInfix = ((\(base,  fs) -> foldl' combine base fs) <$>) 
  where
    combine acc (op, e) =  SInfixExpr $ SInfix acc op e