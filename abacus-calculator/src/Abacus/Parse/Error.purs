module Abacus.Parse.Error
  ( ParseError(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (init, last)
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
      [ "  (Position "
      , show pos
      , ")\n  Unexpected \""
      , fromMaybe "Nothing" actual
      , "\".\n  Error parsing "
      , listWords "or" expected
      , "."
      ]

instance parseErrorSemigroup :: Semigroup ParseError where
  append (ParseError e1) (ParseError e2) = case compare e1.pos e2.pos of
    LT -> ParseError e2
    EQ ->
      ParseError
        { expected: e1.expected <> e2.expected
        , actual: e1.actual <|> e2.actual
        , pos: e1.pos
        }
    GT -> ParseError e1

instance parseErrorMonoid :: Monoid ParseError where
  mempty =
    ParseError
      { expected: []
      , actual: empty
      , pos: 0
      }

listWords :: String -> Array String -> String
listWords _ [ w ] = w

listWords sep [ w1, w2 ] = w1 <> sep <> w2

listWords sep ws =
  intercalate
    ", "
    (init' ws <> [ sep ])
    <> " "
    <> last' ws
  where
  init' xs = fromMaybe [] $ init xs

  last' xs = fromMaybe "Nothing" $ last xs
