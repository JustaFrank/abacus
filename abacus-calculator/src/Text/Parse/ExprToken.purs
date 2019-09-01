module Text.Parse.ExprToken where

import Prelude

import Control.Alt ((<|>))
import Data.Array (fold, many, (:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String (CodePoint, fromCodePointArray)
import Global (readFloat)
import Text.Parse.Base (char, codePoint, parseFloatS', parseWhitespaceS, string)
import Text.Parse.Parser (Parser, anyOf)

---------------------------------------------------------------------------
-- Token

data ExprToken
  = ExprLiteral Number
  | ExprOper Oper
  | ExprFunc Func
  | ExprOpenParen
  | ExprCloseParen
  | ExprComma

derive instance eqExprToken :: Eq ExprToken

derive instance genericExprToken :: Generic ExprToken _

instance showExprToken :: Show ExprToken where
  show = genericShow

newtype Oper = Oper
  { symbol :: CodePoint
  , preced :: Int
  , assoc :: OperAssoc
  , exec :: Number -> Number -> Number
  }

instance operEq :: Eq Oper where
  eq (Oper { symbol: s, preced: p, assoc: a }) (Oper { symbol: s', preced: p', assoc: a' })
    = s == s' && a == a'

instance operShow :: Show Oper where
  show (Oper { symbol: s, preced: p, assoc: a })
    = "(Oper " <> show { symbol: s, preced: p, assoc: a } <> ")"

data OperAssoc
  = LeftAssoc
  | RightAssoc

derive instance operAssocEq :: Eq OperAssoc

derive instance operAssocGeneric :: Generic OperAssoc _

instance operAssocShow :: Show OperAssoc where
  show = genericShow

newtype Func = Func
  { symbol :: String
  , arity :: Int
  , exec :: Array Number -> Maybe Number
  }

instance funcEq :: Eq Func where
  eq (Func { symbol: s, arity: a }) (Func { symbol: s', arity: a' })
    = s == s' && a == a'

instance funcShow :: Show Func where
  show (Func { symbol: s, arity: a })
    = "(Func " <> show { symbol: s, arity: a } <> ")"

---------------------------------------------------------------------------
-- Tokenize

createExprGroupParser :: Array Oper -> Array Func -> Parser (Array ExprToken)
createExprGroupParser opers funcs =
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
    group <- createExprGroupParser opers funcs
    close <- pure <$> parseExprCloseParen
    pure $ open <> group <> close
  parseFuncGroup = (:) <$> parseExprFunc <*> parseParenGroup
  parseExprFunc = createExprFuncParser funcs
  parseExprOper = createExprOperParser opers

---------------------------------------------------------------------------
-- Token Parser Derivatives

createExprOperParser :: Array Oper -> Parser ExprToken
createExprOperParser opers = ExprOper <$> anyOf (map crtPrsr opers)
 where crtPrsr (Oper oper) = Oper oper <$ codePoint oper.symbol

createExprFuncParser :: Array Func -> Parser ExprToken
createExprFuncParser funcs = ExprFunc <$> anyOf (map crtPrser funcs)
 where crtPrser (Func func) = Func func <$ string func.symbol

---------------------------------------------------------------------------
-- Token Parsers

parseExprLiteral :: Parser ExprToken
parseExprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> parseFloatS'

parseExprOpenParen :: Parser ExprToken
parseExprOpenParen = ExprOpenParen <$ char '('

parseExprCloseParen :: Parser ExprToken
parseExprCloseParen = ExprCloseParen <$ char ')'

parseExprComma :: Parser ExprToken
parseExprComma = ExprComma <$ char ','
