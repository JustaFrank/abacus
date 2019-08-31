module Text.Parse.ExprToken where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fold, many, (:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (CodePoint, fromCodePointArray)
import Global (readFloat)
import Text.Parse.Base (char, parseDigitC, parseFloatS', parseLetterC, parseSpecialChar, parseWhitespaceC, parseWhitespaceS)
import Text.Parse.Parser (Parser)

---------------------------------------------------------------------------
-- Token

data ExprToken
  = ExprLiteral Number
  | ExprOper CodePoint
  | ExprFunc String
  | ExprOpenParen
  | ExprCloseParen
  | ExprComma

derive instance eqExprToken :: Eq ExprToken

derive instance genericExprToken :: Generic ExprToken _

instance showExprToken :: Show ExprToken where
  show = genericShow

---------------------------------------------------------------------------
-- Tokenize


parseExprGroup :: Parser (Array ExprToken)
parseExprGroup =
  (<>)
    <$> (fold <$> many parseTermOper)
    <*> (parseWhitespaceS *> parseTerm)
 where
  parseTermOper = do
    term <- parseWhitespaceS *> parseTerm
    oper <-
      parseWhitespaceS *> (pure <$> parseExprOper) <|> (pure <$> parseExprComma)
    pure $ term <> oper
  parseTerm = parseParenGroup <|> parseFuncGroup <|> pure <$> parseExprLiteral
  parseParenGroup = do
    open  <- pure <$> parseExprOpenParen
    group <- parseExprGroup
    close <- pure <$> parseExprCloseParen
    pure $ open <> group <> close
  parseFuncGroup = (:) <$> parseExprFunc <*> parseParenGroup

---------------------------------------------------------------------------
-- Token Parsers

parseExprLiteral :: Parser ExprToken
parseExprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> parseFloatS'

parseExprOper :: Parser ExprToken
parseExprOper = ExprOper <$> parseSpecialChar

parseExprFunc :: Parser ExprToken
parseExprFunc = ExprFunc <<< fromCodePointArray <$> parseFuncName
 where parseFuncName = (:) <$> parseLetterC <*> many (parseLetterC <|> parseDigitC)

parseExprOpenParen :: Parser ExprToken
parseExprOpenParen = ExprOpenParen <$ char '('

parseExprCloseParen :: Parser ExprToken
parseExprCloseParen = ExprCloseParen <$ char ')'

parseExprComma :: Parser ExprToken
parseExprComma = ExprComma <$ char ','
