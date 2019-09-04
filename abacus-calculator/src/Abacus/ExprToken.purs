module Abacus.ExprToken where

import Prelude
import Abacus.Parse.Base (char, codePoint, parseFloatS', parseWhitespaceS, string)
import Abacus.Parse.Parser (Parser, anyOf, (<?>))
import Control.Alt ((<|>))
import Data.Array (fold, many, (:))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.String (CodePoint, fromCodePointArray, singleton)
import Global (readFloat)

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

instance showExprToken :: Show ExprToken where
  show (ExprLiteral n) = show n
  show (ExprOper oper) = show oper
  show (ExprFunc func) = show func
  show ExprOpenParen = "("
  show ExprCloseParen = ")"
  show ExprComma = ","

type ExecFunc
  = Array Number -> Maybe Number

newtype Oper
  = Oper
  { symbol :: CodePoint
  , preced :: Int
  , assoc :: OperAssoc
  , exec :: ExecFunc
  }

instance operEq :: Eq Oper where
  eq (Oper { symbol: s, preced: p, assoc: a }) (Oper { symbol: s', preced: p', assoc: a' }) = s == s' && p == p' && a == a'

instance operShow :: Show Oper where
  show (Oper { symbol }) = singleton symbol

data OperAssoc
  = LeftAssoc
  | RightAssoc

derive instance operAssocEq :: Eq OperAssoc

derive instance operAssocGeneric :: Generic OperAssoc _

instance operAssocShow :: Show OperAssoc where
  show = genericShow

newtype Func
  = Func
  { symbol :: String
  , arity :: Int
  , exec :: ExecFunc
  }

instance funcEq :: Eq Func where
  eq (Func { symbol: s, arity: a }) (Func { symbol: s', arity: a' }) = s == s' && a == a'

instance funcShow :: Show Func where
  show (Func { symbol }) = symbol

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
    open <- pure <$> parseExprOpenParen
    group <- createExprGroupParser opers funcs
    close <- pure <$> (parseWhitespaceS *> parseExprCloseParen)
    pure $ open <> group <> close

  parseFuncGroup = (:) <$> (parseExprFunc <* parseWhitespaceS) <*> parseParenGroup

  parseExprFunc = createExprFuncParser funcs

  parseExprOper = createExprOperParser opers

---------------------------------------------------------------------------
-- Token Parser Derivatives
createExprOperParser :: Array Oper -> Parser ExprToken
createExprOperParser opers =
  ExprOper
    <$> anyOf (map crtPrsr opers)
    <?> "operator"
  where
  crtPrsr (Oper oper) = Oper oper <$ codePoint oper.symbol

createExprFuncParser :: Array Func -> Parser ExprToken
createExprFuncParser funcs =
  ExprFunc
    <$> anyOf (map crtPrser funcs)
    <?> "function"
  where
  crtPrser (Func func) = Func func <$ string func.symbol

---------------------------------------------------------------------------
-- Token Parsers
parseExprLiteral :: Parser ExprToken
parseExprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> parseFloatS'
    <?> "number"

parseExprOpenParen :: Parser ExprToken
parseExprOpenParen = ExprOpenParen <$ char '(' <?> "open parentheses"

parseExprCloseParen :: Parser ExprToken
parseExprCloseParen = ExprCloseParen <$ char ')' <?> "close parentheses"

parseExprComma :: Parser ExprToken
parseExprComma = ExprComma <$ char ',' <?> "comma"
