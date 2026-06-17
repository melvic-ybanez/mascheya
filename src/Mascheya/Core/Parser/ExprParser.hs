module Mascheya.Core.Parser.ExprParser where

import Mascheya.Core.Ast.Source 
import Control.Applicative ((<|>))
import Mascheya.Core.Parser.Core (Parser, parseMap, (<&>), repeat, track)
import Mascheya.Core.Parser.Primitives 
import Mascheya.Core.Result (Loc(Loc))
import qualified Mascheya.Core.Lexemes as Lexemes
import Prelude hiding (repeat)
import Data.List.NonEmpty (fromList)

sExpr :: Parser SExpr
sExpr = expr <|> inParens expr
  where 
    expr = app <|> nonApp
    app = SAppExpr <$> sApp

sApp :: Parser SApp
sApp = (\((f, a), l) -> SApp f a $ Loc l) <$> (track $ callable' <&> args')
  where 
    callable' = nonApp <|> (inParens $ SAppExpr <$> sApp)
    args' = fromList <$> repeat arg
    arg = snd <$> char Lexemes.space <&> sExpr

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