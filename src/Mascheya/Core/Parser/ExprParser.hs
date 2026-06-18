module Mascheya.Core.Parser.ExprParser where

import Mascheya.Core.Ast.Source 
import Control.Applicative ((<|>))
import Mascheya.Core.Parser.Core (Parser, parseMap, (<&>), repeat, track, char)
import Mascheya.Core.Parser.Primitives 
import Mascheya.Core.Result (Loc(Loc))
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (repeat)
import Data.List.NonEmpty (fromList)

sExpr :: Parser SExpr
sExpr = expr <|> inParens expr
  where 
    expr = arith <|> app <|> nonApp
    app = SAppExpr <$> sApp
    arith = SBuiltinFuncExpr . SArithFunc <$> sArith

sApp :: Parser SApp
sApp = (\((f, a), l) -> SApp f a $ Loc l) <$> (track $ callable' <&> args')
  where 
    callable' = nonApp <|> (inParens $ SAppExpr <$> sApp)
    args' = fromList <$> repeat sArg

sArith :: Parser SArith
sArith = toArith <$> sArithKind <&> args'
  where
    toArith (arithKind, (expr1, expr2)) = SArith arithKind expr1 expr2
    args' = sArg <&> sArg

sArg :: Parser SExpr
sArg = snd <$> char Lexemes.space <&> sExpr

nonApp :: Parser SExpr
nonApp = nonApp' <|> inParens nonApp'
  where 
    nonApp' = variable <|> literal
    literal = SLitExpr <$> sLit
    variable = SVarExpr <$> sVar

sVar :: Parser SVar
sVar = (\(name, line') -> SVar name $ Loc line') <$> functionId

sLit :: Parser SLit
sLit = numLit <|> charLit <|> boolLit 
  where
    numLit = SNumLit <$> sNum
    charLit = SCharLit <$> sChar
    boolLit = SBoolLit <$> sBool

sNum :: Parser SNum
sNum = floatLit <|> doubleLit <|> intLit
  where
    intLit = SIntNum <$> sInt
    doubleLit = SDoubleNum <$> sDouble
    floatLit = SFloatNum <$> sFloat

sInt :: Parser SInt
sInt = parseMap SInt int

sDouble :: Parser SDouble
sDouble = parseMap SDouble double

sFloat :: Parser SFloat
sFloat = parseMap SFloat float

sChar :: Parser SChar
sChar = (\((_, content), _) -> SChar content) <$> singleQuote 
  <&> (escaped <|> unescaped) <&> singleQuote

sBool :: Parser SBool
sBool = sTrue <|> sFalse
  where
    sTrue = (const STrue) <$> true
    sFalse = (const SFalse) <$> false

sArithKind :: Parser SArithKind
sArithKind = sPlus <|> sMinus <|> sTimes <|> sDivide <|> sModulo

sPlus :: Parser SArithKind
sPlus = const SPlus <$> char Lexemes.plus 

sMinus :: Parser SArithKind
sMinus = const SMinus <$> char Lexemes.minus

sTimes :: Parser SArithKind
sTimes = const STimes <$> char Lexemes.times

sDivide :: Parser SArithKind
sDivide = const SDivide <$> char Lexemes.divide

sModulo :: Parser SArithKind
sModulo = const SModulo <$> char Lexemes.modulo