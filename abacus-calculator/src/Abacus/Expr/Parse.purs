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
import Abacus.Expr.Token.Default (multO)
import Abacus.Parse (Parser, eof, pNot, whitespace)
import Abacus.Utils.ReaderT (applyReaderT, deferReaderT)
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Control.Monad.Reader (ReaderT, lift, mapReaderT, runReaderT, withReaderT)
import Data.Array (intercalate, many, (:))

type ExprParser a
  = ReaderT ExprEnv Parser a

runParseExpr :: ExprEnv -> Parser TokenStack
runParseExpr env = runReaderT parseExpr env

parseExpr :: ExprParser TokenStack
parseExpr =
  mapReaderT (many whitespace *> _)
    $ (_ <* eof)
        `mapReaderT`
          deferReaderT (\_ -> parseDeclaration <|> parseExprGroup)

parseExprGroup :: ExprParser TokenStack
parseExprGroup =
  sepByConcat
    `mapReaderT`
      deferReaderT (\_ -> parseProduct <|> parseTerm)
    `applyReaderT`
      (_.opers `withReaderT` exprOper)

parseTerm :: ExprParser TokenStack
parseTerm =
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
  t <- notConsecLit `mapReaderT` deferReaderT (\_ -> parseTerm)
  ts <- someP `mapReaderT` deferReaderT (\_ -> parseTerm)
  pure $ intercalate [ ExprOper multO ] (t : ts)
  where
  someP p = (:) <$> notConsecLit p <*> defer (\_ -> manyP p)

  manyP p = (someP p <|> pure [])

  notConsecLit p = pNot (exprLiteral *> exprLiteral) *> p

parseParenGroup :: ExprParser TokenStack
parseParenGroup = parenConcat `mapReaderT` deferReaderT (\_ -> parseExprGroup)

parseFuncGroup :: ExprParser TokenStack
parseFuncGroup =
  (:)
    <$> _.funcs `withReaderT` exprFunc
    <*> deferReaderT (\_ -> parseCommaGroup)

parseCommaGroup :: ExprParser TokenStack
parseCommaGroup =
  mapReaderT parenConcat
    $ sepByConcat
        `mapReaderT`
          deferReaderT (\_ -> parseExprGroup)
        `applyReaderT`
          sep
  where
  sep = _.opers `withReaderT` exprOper <|> lift exprComma
