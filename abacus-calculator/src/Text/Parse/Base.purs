module Text.Parse.Base where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, singleton, uncons)
import Data.Traversable (sequence)

import Text.Parse.Parser (Parser(..))

codePointArray :: Array CodePoint -> Parser (Array CodePoint)
codePointArray xs = parseCodePointArray
  where
    parseCodePointArray = sequence <<< map codePoint $ xs

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
