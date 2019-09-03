module Abacus.Parse.Base where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many, some, (:))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, singleton, toCodePointArray, uncons)
import Data.Traversable (sequence)
import Abacus.Parse.CharSets (digits, letters, specialChars, whitespaces)
import Abacus.Parse.Parser (Parser(..), anyOf)

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
parseLetterC = anyOf $ map codePoint letters

parseDigitC :: Parser CodePoint
parseDigitC = anyOf $ map codePoint digits

parseWhitespaceC :: Parser CodePoint
parseWhitespaceC = anyOf $ map codePoint whitespaces

parseSpecialChar :: Parser CodePoint
parseSpecialChar = anyOf $ map codePoint specialChars

---------------------------------------------------------------------------
-- Parser Derivatives

char :: Char -> Parser CodePoint
char = codePoint <<< codePointFromChar

string :: String -> Parser (Array CodePoint)
string = codePointArray <<< toCodePointArray

codePoint :: CodePoint -> Parser CodePoint
codePoint c = Parser parseCodePoint
 where
  parseCodePoint s = case uncons s of
    Nothing ->
      Left $ "Unexpected EOF"
    Just { head: x, tail: xs }
      | x == c -> Right { rest: xs, token: x }
      | otherwise -> Left $ fold
        ["Expected ", singleton c, " but got ", singleton x]

codePointArray :: Array CodePoint -> Parser (Array CodePoint)
codePointArray xs = parseCodePointArray
  where
    parseCodePointArray = sequence <<< map codePoint $ xs
