module Text.Parse.CharSets
  ( digits
  , letters
  , specialChars
  , whitespaces
  ) where

import Prelude

import Data.Enum (enumFromTo)
import Data.String (CodePoint, codePointFromChar)

digits :: Array CodePoint
digits = map codePointFromChar $ enumFromTo '0' '9'

letters :: Array CodePoint
letters = map codePointFromChar $ enumFromTo 'a' 'Z'

whitespaces :: Array CodePoint
whitespaces = map codePointFromChar $ [' ', '\n', '\r']

specialChars :: Array CodePoint
specialChars =
  map codePointFromChar
    $ [ '!'
      , '#'
      , '$'
      , '%'
      , '*'
      , '+'
      , '/'
      , '<'
      , '='
      , '>'
      , '?'
      , '@'
      , '\\'
      , '^'
      , '|'
      , '-'
      , '~'
      , ':'
      , ';'
      ]
