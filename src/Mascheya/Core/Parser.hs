module Mascheya.Core.Parser where

import Mascheya.Core.Token
import Mascheya.Core.Parser.ParseResult (ParseResult, succeed, fromStep, mapValue)
import Mascheya.Core.Parser.Types
import qualified Mascheya.Core.Token as T
import qualified Mascheya.Core.Result as Result
import qualified Mascheya.Core.Ast.Source as S
import Mascheya.Core.Ast.Source (Expr(..))
import qualified Mascheya.Core.Parser.ParseResult as ParseResult
import Mascheya.Core.Result (expectedError)

fromTokens :: [Token] -> Parser
fromTokens = flip Parser 0

parse :: Parser -> ParseResult [Expr]
parse parser | isAtEnd parser = succeed [] parser
parse parser = mapValue (\expr -> [expr]) $ parseExpr parser

parseExpr :: Parser -> ParseResult Expr
parseExpr parser = handle $ fmap fromStep $ parseLiteral parser
    where handle Nothing = ParseResult.fail error parser
          handle (Just result) = mapValue (\r -> S.Literal r) result

          error = Result.ParseError $ expectedError (peek parser) "expression" "at start"

parseLiteral :: Parser -> Maybe (Step S.Literal)
parseLiteral = fmap next . matchAnyWith pred
    where pred (T.Literal _) = True
          pred _ = False

          next result = Step (makeLiteral $ tokenType $ previousToken result) result
            where makeLiteral (T.Literal (T.Int value)) = S.SInt value

matchAny :: [TokenType] -> Parser -> Maybe Parser
matchAny tokenTypes = matchAnyWith (\tokenType -> elem tokenType tokenTypes)

matchAnyWith :: (TokenType -> Bool) -> Parser -> Maybe Parser
matchAnyWith pred parser = processResult $ checkWith pred parser
    where processResult True = Just $ next $ advance parser
          processResult False = Nothing

checkWith :: (TokenType -> Bool) -> Parser -> Bool 
checkWith pred parser = if isAtEnd parser then False else pred $ tokenType $ peek parser

check :: TokenType -> Parser -> Bool
check tokenType = checkWith $ \tt -> tt == tokenType

isAtEnd :: Parser -> Bool
isAtEnd parser = tokenType (peek parser) == Eof

peek :: Parser -> Token
peek parser = tokens parser !! current parser

previousToken :: Parser -> Token
previousToken parser = tokens parser !! (current parser - 1)

advance :: Parser -> Step Token
advance parser = if isAtEnd parser then Step (previousToken parser) parser 
    else Step (previousToken newParser) newParser
        where newParser = parser { current = current parser + 1 }