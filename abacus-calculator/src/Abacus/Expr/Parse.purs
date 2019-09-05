module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Token (ExprToken(..), Func, Oper)
import Abacus.Parse (Parser, char, eof, fail, floatS', lexeme, specialChar, whitespace, word, (<?>))
import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.Array (many, (:))
import Data.Foldable (find, fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (fromCodePointArray)
import Data.String as S
import Global (readFloat)

-- | Type that stores the environment for the expression parser.
type ExprEnv
  = { opers :: Array Oper
    , funcs :: Array Func
    }

-- | NOTE: THESE FUNCTIONS CAN USE SIGNIFICANT CLEANUP WITH `between`,
-- | `sepBy`, and potentially MANY OTHER COMBINATORS!
--
-- | Parses an expression.
expr :: ExprEnv -> Parser (Array ExprToken)
expr env = many whitespace *> exprGroup env <* eof

-- | Parses an expression group, which consists of a term, followed by
-- | any number of operator-term pairs.
exprGroup :: ExprEnv -> Parser (Array ExprToken)
exprGroup env = lift2 (<>) term (fold <$> many operTerm)
  where
  term = parenGroup env <|> funcGroup env <|> pure <$> exprLiteral

  operTerm = lift2 (:) (exprOper env.opers) term

-- | Parses a parenthesized expression group.
parenGroup :: ExprEnv -> Parser (Array ExprToken)
parenGroup env = do
  op <- exprOpenParen
  grp <- exprGroup env
  cp <- exprCloseParen
  pure $ [ op ] <> grp <> [ cp ]

-- | Parses a function followed by a comma group.
funcGroup :: ExprEnv -> Parser (Array ExprToken)
funcGroup env = (:) <$> lexeme (exprFunc env.funcs) <*> parenCommaGroup env

-- | Parses a parenthesized group of terms separated by commas.
parenCommaGroup :: ExprEnv -> Parser (Array ExprToken)
parenCommaGroup env = do
  op <- exprOpenParen
  grp <- commaGroup env
  cp <- exprCloseParen
  pure $ [ op ] <> grp <> [ cp ]

-- | Parses a group of terms separated by commas.
commaGroup :: ExprEnv -> Parser (Array ExprToken)
commaGroup env = lift2 (<>) term (fold <$> many operTerm)
  where
  term = parenGroup env <|> funcGroup env <|> pure <$> exprLiteral

  operTerm = lift2 (:) (exprOper env.opers <|> exprComma) term

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
