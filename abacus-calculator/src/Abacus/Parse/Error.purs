module Abacus.Parse.Error
  ( ParseError(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe, fromMaybe)

newtype ParseError
  = ParseError
  { expected :: Array String
  , actual :: Maybe String
  , pos :: Int
  }

derive instance parseErrorEq :: Eq ParseError

instance parseErrorShow :: Show ParseError where
  show (ParseError { expected, actual, pos }) =
    fold
      [ "  position: "
      , show pos
      , "\n  unexpected: "
      , fromMaybe "Nothing" actual
      , "\n  expected: "
      , intercalate ", " expected
      ]

instance parseErrorSemigroup :: Semigroup ParseError where
  append (ParseError e1) (ParseError e2) =
    ParseError
      { expected: e1.expected <> e2.expected
      , actual: e1.actual <|> e2.actual
      , pos: max e1.pos e2.pos
      }

instance parseErrorMonoid :: Monoid ParseError where
  mempty = ParseError { expected: [], actual: empty, pos: 0 }
