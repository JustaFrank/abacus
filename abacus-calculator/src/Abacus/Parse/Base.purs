module Abacus.Parse.Base where

import Prelude
import Abacus.Parse.CharSets (digits, letters, specialChars, whitespaces)
import Abacus.Parse.Error (ParseError(..))
import Abacus.Parse.Parser (Parser(..), anyOf, labelParser)
import Control.Alt ((<|>))
import Data.Array (many, some, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, singleton, toCodePointArray, uncons)
import Data.Traversable (sequence)

---------------------------------------------------------------------------
-- Basic String Parsers
-- Parses a natural number.
parseNatS :: Parser (Array CodePoint)
parseNatS = some parseDigitC

parseIntS :: Parser (Array CodePoint)
parseIntS = do
  sign <- string "-" <|> pure mempty
  num <- parseNatS
  pure $ sign <> num

-- Parses a decimal point and the following digits.
parseDecimalS :: Parser (Array CodePoint)
parseDecimalS = (:) <$> char '.' <*> parseNatS

parseFloatS :: Parser (Array CodePoint)
parseFloatS = do
  int <- parseIntS
  decimal <- parseDecimalS <|> string "." <|> pure mempty
  pure $ int <> decimal

-- Same as `parseFloatS` but assumes leading 0 (i.e. .1234 is valid).
parseFloatS' :: Parser (Array CodePoint)
parseFloatS' = parseFloatS <|> parseDecimalS

parseWhitespaceS :: Parser (Array CodePoint)
parseWhitespaceS = many parseWhitespaceC

---------------------------------------------------------------------------
-- Basic Character Parsers
parseLetterC :: Parser CodePoint
parseLetterC = labelParser (anyOf $ map codePoint letters) "letter"

parseDigitC :: Parser CodePoint
parseDigitC = labelParser (anyOf $ map codePoint digits) "digit"

parseWhitespaceC :: Parser CodePoint
parseWhitespaceC =
  labelParser
    (anyOf $ map codePoint whitespaces)
    "whitespace"

parseSpecialChar :: Parser CodePoint
parseSpecialChar =
  labelParser
    (anyOf $ map codePoint specialChars)
    "special char"

---------------------------------------------------------------------------
-- Parser Derivatives
char :: Char -> Parser CodePoint
char = codePoint <<< codePointFromChar

string :: String -> Parser (Array CodePoint)
string s = labelParser (codePointArray <<< toCodePointArray $ s) s

codePoint :: CodePoint -> Parser CodePoint
codePoint c = Parser parseCodePoint
  where
  parseCodePoint s = case uncons s of
    Nothing ->
      Left
        $ ParseError
            { expected: [ singleton c ], actual: Just "end of input", pos: 0
            }
    Just { head: x, tail: xs }
      | x == c -> Right { input: xs, result: x }
      | otherwise ->
        Left
          $ ParseError
              { expected: [ singleton c ], actual: Just $ singleton x, pos: 0 }

codePointArray :: Array CodePoint -> Parser (Array CodePoint)
codePointArray xs = parseCodePointArray
  where
  parseCodePointArray = sequence <<< map codePoint $ xs
