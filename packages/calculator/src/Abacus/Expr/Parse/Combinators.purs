module Abacus.Expr.Parse.Combinators where

import Prelude
import Abacus.Expr.Parse.Token (exprCloseParen, exprOpenParen)
import Abacus.Expr.Token (ExprToken)
import Abacus.Parse (Parser)
import Data.Array (many, (:))

parenConcat :: Parser (Array ExprToken) -> Parser (Array ExprToken)
parenConcat = betweenConcat (pure <$> exprOpenParen) (pure <$> exprCloseParen)

sepByConcat :: forall a. Parser (Array a) -> Parser a -> Parser (Array a)
sepByConcat p sep = (<>) <$> p <*> (join <$> many ((:) <$> sep <*> p))

betweenConcat ::
  forall a. Semigroup a => Parser a -> Parser a -> Parser a -> Parser a
betweenConcat open close p = do
  o <- open
  x <- p
  c <- close
  pure $ o <> x <> c
