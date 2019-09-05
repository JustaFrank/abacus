module Abacus.Parse.Char
  ( char
  , codePoint
  , digit
  , letter
  , satisfyC
  , specialChar
  , whitespace
  ) where

import Prelude
import Abacus.Parse.Base (satisfy)
import Abacus.Parse.Parser (Parser, (<?>))
import Data.Char.Unicode (isDigit, isLetter)
import Data.String (CodePoint, codePointFromChar)
import Data.String as S
import Data.String.Unsafe as SU
import Data.Traversable (oneOf)

whitespace :: Parser CodePoint
whitespace = oneOf (map char [ '\n', '\r', ' ', '\t' ]) <?> "whitespace"

letter :: Parser CodePoint
letter = satisfyC isLetter <?> "letter"

digit :: Parser CodePoint
digit = satisfyC isDigit <?> "digit"

specialChar :: Parser CodePoint
specialChar =
  oneOf
    (map char [ '+', '-', '*', '/', '^', '=', '%', '$' ])
    <?> "special character"

-- | Parses a single code point.
char :: Char -> Parser CodePoint
char = codePoint <<< codePointFromChar

codePoint :: CodePoint -> Parser CodePoint
codePoint c = satisfy (_ == c) <?> S.singleton c

-- | Note: `char` is unsafe.
satisfyC :: (Char -> Boolean) -> Parser CodePoint
satisfyC pred = satisfy $ pred <<< SU.char <<< S.singleton
