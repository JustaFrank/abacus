module Text.Parse.ExprToken where

import Prelude

import Data.Array (many)
import Data.String (fromCodePointArray)
import Global (readFloat)
import Text.Parse.Base (parseFloatS', parseSpecialChar)
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
    <$> parseFloatS'

parseExprOper :: Parser ExprToken
parseExprOper = ExprOper <<< fromCodePointArray <$> many parseSpecialChar
