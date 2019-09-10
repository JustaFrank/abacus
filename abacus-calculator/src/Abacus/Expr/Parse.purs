module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Parse.Combinators (parenConcat, sepByConcat)
import Abacus.Expr.Parse.Token
  ( exprComma
  , exprEq
  , exprFunc
  , exprLiteral
  , exprOper
  , exprSymb
  , exprVar
  )
import Abacus.Expr.Token (ExprEnv, ExprToken(..), TokenStack)
import Abacus.Expr.Token.Default (mult)
import Abacus.Parse (Parser, eof, pNot, whitespace)
import Abacus.Utils.ReaderT (applyReaderT, deferReaderT)
import Control.Alternative ((<|>))
import Control.Monad.Reader (ReaderT, lift, mapReaderT, runReaderT, withReaderT)
import Data.Array (intercalate, many, (:))

type ExprParser a
  = ReaderT ExprEnv Parser a

runParseExpr :: ExprEnv -> Parser TokenStack
runParseExpr env = many whitespace *> runReaderT parseExpr env <* eof

parseExpr :: ExprParser TokenStack
parseExpr =
  (_ <* eof)
    `mapReaderT`
      deferReaderT (\_ -> parseDeclaration <|> parseExprGroup)

parseExprGroup :: ExprParser TokenStack
parseExprGroup =
  sepByConcat
    `mapReaderT`
      deferReaderT (\_ -> parseProduct <|> term)
    `applyReaderT`
      (_.opers `withReaderT` exprOper)

term :: ExprParser TokenStack
term =
  pure <$> lift exprLiteral
    <|> (pure <$> _.vars `withReaderT` exprVar)
    <|> deferReaderT (\_ -> parseParenGroup)
    <|> deferReaderT (\_ -> parseFuncGroup)

parseDeclaration :: ExprParser TokenStack
parseDeclaration = do
  s <- lift exprSymb
  eq <- lift exprEq
  x <- parseExpr
  pure $ s : eq : x

parseProduct :: ExprParser TokenStack
parseProduct = do
  _ <- mapReaderT pNot $ exprLiteralR *> exprLiteralR
  ts <- mapReaderT many term
  pure $ intercalate [ ExprOper mult ] ts
  where
  exprLiteralR = lift exprLiteral

parseParenGroup :: ExprParser TokenStack
parseParenGroup = parenConcat `mapReaderT` deferReaderT (\_ -> parseExprGroup)

parseFuncGroup :: ExprParser TokenStack
parseFuncGroup =
  (:)
    <$> _.funcs `withReaderT` exprFunc
    <*> deferReaderT (\_ -> parseCommaGroup)

parseCommaGroup :: ExprParser TokenStack
parseCommaGroup =
  sepByConcat
    `mapReaderT`
      deferReaderT (\_ -> parseExprGroup)
    `applyReaderT`
      sep
  where
  sep = _.opers `withReaderT` exprOper <|> lift exprComma
