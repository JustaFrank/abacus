module Text.Parse.Base
  ( char
  , codePoint
  , codePointArray
  , parseDigit
  , parseLetter
  , parseWhitespace
  , string
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar, singleton, toCodePointArray, uncons)
import Data.Traversable (sequence)

import Text.Parse.CharSets (digits, letters, whitespaces)
import Text.Parse.Parser (Parser(..), anyOf)

---------------------------------------------------------------------------
-- Basic Parsers

parseLetter :: Parser CodePoint
parseLetter = anyOf $ map codePoint letters

parseDigit :: Parser CodePoint
parseDigit = anyOf $ map codePoint digits

parseWhitespace :: Parser CodePoint
parseWhitespace = anyOf $ map codePoint whitespaces

---------------------------------------------------------------------------
-- Parser Derivatives

codePoint :: CodePoint -> Parser CodePoint
codePoint c = Parser parseCodePoint
 where
  parseCodePoint s = case uncons s of
    Nothing ->
      Left $ pure "Unexpected EOF"
    Just { head: x, tail: xs }
      | x == c -> Right { rest: xs, token: x }
      | otherwise -> Left $ pure $ fold
        ["Expected ", singleton c, " but got ", singleton x]

codePointArray :: Array CodePoint -> Parser (Array CodePoint)
codePointArray xs = parseCodePointArray
  where
    parseCodePointArray = sequence <<< map codePoint $ xs

char :: Char -> Parser CodePoint
char = codePoint <<< codePointFromChar

string :: String -> Parser (Array CodePoint)
string = codePointArray <<< toCodePointArray
