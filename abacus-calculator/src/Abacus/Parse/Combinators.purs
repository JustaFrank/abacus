module Abacus.Parse.Combinators where

import Prelude
import Abacus.Parse.Parser (Parser)
import Control.Alternative ((<|>))
import Control.Lazy (defer)
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

sepByITill ::
  forall a b.
  Semigroup a =>
  Parser a -> Parser b -> Parser a -> Parser (Array a)
sepByITill sep end p = (:) <$> p <*> manyTill ((<>) <$> sep <*> p) end

manyTill :: forall a b. Parser a -> Parser b -> Parser (Array a)
manyTill p end = ([] <$ end) <|> defer (\_ -> someTill p end)

someTill :: forall a b. Parser a -> Parser b -> Parser (Array a)
someTill p end = (:) <$> p <*> manyTill p end
