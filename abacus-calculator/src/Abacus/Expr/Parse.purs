module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Default (mult)
import Abacus.Expr.Parse.Token
  ( exprCloseParen
  , exprComma
  , exprEq
  , exprFunc
  , exprLiteral
  , exprOpenParen
  , exprOper
  , exprSymb
  , exprVar
  )
import Abacus.Expr.Token (ExprEnv, ExprToken(..), TokenStack)
import Abacus.Parse (Parser, betweenI, eof, pNot, whitespace)
import Control.Alternative ((<|>))
import Control.Lazy (class Lazy, defer)
import Control.Monad.Reader (ReaderT(..), lift, mapReaderT, runReaderT, withReaderT)
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

parenConcat :: Parser (Array ExprToken) -> Parser (Array ExprToken)
parenConcat = betweenI (pure <$> exprOpenParen) (pure <$> exprCloseParen)

sepByConcat :: forall a. Parser (Array a) -> Parser a -> Parser (Array a)
sepByConcat p sep = (<>) <$> p <*> (join <$> many ((:) <$> sep <*> p))

applyReaderT ::
  forall r m a b.
  Monad m =>
  ReaderT r (Function (m a)) (m b) -> ReaderT r m a -> ReaderT r m b
applyReaderT rdr1 rdr2 = ReaderT $ \r -> runReaderT (runReaderT rdr1 r `mapReaderT` rdr2) r

deferReaderT ::
  forall r m a.
  Monad m =>
  Lazy (m a) => (Unit -> ReaderT r m a) -> ReaderT r m a
deferReaderT rdr = ReaderT $ \r -> defer (\_ -> runReaderT (rdr unit) r)
