module Text.Parse.Base where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, singleton, uncons)

import Text.Parse.Parser (Parser(..))

char :: CodePoint -> Parser CodePoint
char c = Parser pchar
 where
  pchar s = case uncons s of
    Nothing ->
      Left $ pure "Unexpected EOF"
    Just { head: x, tail: xs }
      | x == c -> Right { rest: xs, token: x }
      | otherwise -> Left $ pure $ fold ["Expected ", singleton c, " but got ", singleton x]
