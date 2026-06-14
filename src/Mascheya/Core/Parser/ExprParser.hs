module Mascheya.Core.Parser.ExprParser where

import Mascheya.Core.Ast.Source 
import Control.Applicative ((<|>))
import qualified Mascheya.Core.Lexemes as Lexemes
import Mascheya.Core.Parser.Core (Parser, parseMap, (<&>))
import Mascheya.Core.Parser.Primitives 

sLiteral :: Parser Literal
sLiteral = numLit <|> charLit <|> boolLit 
  where
    numLit = NumLit <$> sNum
    charLit = CharLit <$> sChar
    boolLit = BoolLit <$> sBool

sNum :: Parser SNum
sNum = floatLit <|> doubleLit <|> intLit
  where
    intLit = IntLit <$> sInt
    doubleLit = DoubleLit <$> sDouble
    floatLit = FloatLit <$> sFloat

sInt :: Parser SInt
sInt = parseMap SInt int

sDouble :: Parser SDouble
sDouble = parseMap SDouble double

sFloat :: Parser SFloat
sFloat = parseMap SFloat float

sChar :: Parser SChar
sChar = (\((_, content), _) -> SChar content) <$> char Lexemes.singleQuote 
  <&> (escaped <|> unescaped) <&> char Lexemes.singleQuote

sBool :: Parser SBool
sBool = sTrue <|> sFalse
  where
    sTrue = (const STrue) <$> true
    sFalse = (const SFalse) <$> false