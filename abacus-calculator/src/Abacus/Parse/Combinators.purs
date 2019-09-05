module Abacus.Parse.Combinators where

import Prelude
import Abacus.Parse.Parser (Parser)
import Data.Array (many, (:))

betweenI ::
  forall a. Semigroup a => Parser a -> Parser a -> Parser a -> Parser a
betweenI open close p = do
  o <- open
  x <- p
  c <- close
  pure $ o <> x <> c

sepByI :: forall a. Semigroup a => Parser a -> Parser a -> Parser (Array a)
sepByI sep p = (:) <$> p <*> many ((<>) <$> sep <*> p)
