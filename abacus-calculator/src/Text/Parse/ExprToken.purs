module Text.Parse.ExprToken where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:), many)
import Data.String (fromCodePointArray)
import Global (readFloat)

import Text.Parse.Base (char, parseDigit, string)
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
-- Token Parsers

parseExprLiteral :: Parser ExprToken
parseExprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> parseFloat
 where
  parseFloat = do
    int <- parseInt
    decimal <- (:) <$> char '.' <*> parseDigits <|> pure mempty
    pure $ int <> decimal
  parseInt = do
    sign <- string "-" <|> pure mempty
    num <- parseDigits
    pure $ sign <> num
  parseDigits = many parseDigit
