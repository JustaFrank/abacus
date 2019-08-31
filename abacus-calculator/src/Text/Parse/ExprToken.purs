module Text.Parse.ExprToken where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:), many, some)
import Data.String (fromCodePointArray)
import Global (readFloat)
import Text.Parse.Base (char, parseDigitC, parseFloatS', parseLetterC, parseSpecialChar, parseWhitespaceC)
import Text.Parse.Parser (Parser)

---------------------------------------------------------------------------
-- Token

data ExprToken
  = ExprLiteral Number
  | ExprOper String
  | ExprFunc String
  | ExprOpenParen
  | ExprCloseParen

derive instance eqExprToken :: Eq ExprToken

---------------------------------------------------------------------------
-- Tokenize

tokenize :: Parser (Array ExprToken)
tokenize = many parseExprToken
 where
  parseExprToken =
    (many parseWhitespaceC)
      *> (   parseExprLiteral
         <|> parseExprOper
         <|> parseExprFunc
         <|> parseExprOpenParen
         <|> parseExprCloseParen
         )

---------------------------------------------------------------------------
-- Token Parsers

parseExprLiteral :: Parser ExprToken
parseExprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> parseFloatS'

parseExprOper :: Parser ExprToken
parseExprOper = ExprOper <<< fromCodePointArray <$> some parseSpecialChar

parseExprFunc :: Parser ExprToken
parseExprFunc = ExprFunc <<< fromCodePointArray <$> parseFuncName
 where parseFuncName = (:) <$> parseLetterC <*> many (parseLetterC <|> parseDigitC)

parseExprOpenParen :: Parser ExprToken
parseExprOpenParen = ExprOpenParen <$ char '('

parseExprCloseParen :: Parser ExprToken
parseExprCloseParen = ExprOpenParen <$ char ')'
