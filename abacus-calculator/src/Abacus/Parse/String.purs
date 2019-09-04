module Abacus.Parse.String where

import Prelude
import Abacus.Parse.Char (char, digit, string)
import Abacus.Parse.Parser (Parser, (<?>))
import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.Array as A
import Data.Array ((:))
import Data.String (CodePoint)

floatS :: Parser (Array CodePoint)
floatS = do
  n <- integerS
  d <- decimalS <|> string "." <|> pure mempty
  pure (n <> d) <?> "float"

-- | Same as `parseFloatS` but assumes leading 0 (i.e. .1234 is valid).
floatS' :: Parser (Array CodePoint)
floatS' = (floatS <|> decimalS) <?> "float"

-- | Parses a decimal point followed by digits.
decimalS :: Parser (Array CodePoint)
decimalS = lift2 (:) (char '.') naturalS <?> "decimal"

integerS :: Parser (Array CodePoint)
integerS = do
  sign <- string "-" <|> pure mempty
  n <- naturalS
  pure (sign <> n) <?> "integer"

naturalS :: Parser (Array CodePoint)
naturalS = A.some digit <?> "natural number"
