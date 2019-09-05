module Abacus.Expr.Parse.Token
  ( exprCloseParen
  , exprComma
  , exprFunc
  , exprLiteral
  , exprOpenParen
  , exprOper
  ) where

import Prelude
import Abacus.Expr.Token (ExprToken(..), Func, Oper)
import Abacus.Parse (Parser, char, fail, floatS', lexeme, specialChar, word, (<?>))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (fromCodePointArray)
import Data.String as S
import Global (readFloat)

-- | Parses an `ExprOper` from an array of `Func`.
exprOper :: Array Oper -> Parser ExprToken
exprOper os = do
  c <- lexeme specialChar <?> "operator"
  let
    x = find (unwrap >>> _.symbol >>> (_ == c)) os
  case x of
    Nothing -> fail $ S.singleton c <> " is not a valid operator"
    Just o -> pure $ ExprOper o

-- | Parses an `ExprFunc` from an array of `Func`.
exprFunc :: Array Func -> Parser ExprToken
exprFunc fs = do
  s <- fromCodePointArray <$> lexeme word <?> "function"
  let
    x = find (unwrap >>> _.symbol >>> (_ == s)) fs
  case x of
    Nothing -> fail $ s <> " is not a valid function"
    Just f -> pure $ ExprFunc f

exprLiteral :: Parser ExprToken
exprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> lexeme floatS'
    <?> "number"

exprOpenParen :: Parser ExprToken
exprOpenParen = ExprOpenParen <$ lexeme (char '(') <?> "\"(\""

exprCloseParen :: Parser ExprToken
exprCloseParen = ExprCloseParen <$ lexeme (char ')') <?> "\")\""

exprComma :: Parser ExprToken
exprComma = ExprComma <$ lexeme (char ',') <?> "comma"
