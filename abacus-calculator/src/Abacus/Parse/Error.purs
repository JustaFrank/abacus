module Abacus.Parse.Error
  ( ParseError(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (fold, intercalate, (:))
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)

data ParseError
  = BasicError
    { expt :: Array String
    , unexpt :: Maybe String
    , pos :: Int
    }
  | CustomError
    { msgs :: Array String
    , pos :: Int
    }

derive instance parseErrorEq :: Eq ParseError

instance parseErrorShow :: Show ParseError where
  show (BasicError { expt, unexpt, pos }) =
    indent 2
      [ showPos pos
      , "Unexpected \"" <> fromMaybe "Nothing" unexpt <> "\"."
      , "Expected " <> listWords "or" expt <> "."
      ]
  show (CustomError { msgs, pos }) = indent 2 (showPos pos : msgs)

indent :: Int -> Array String -> String
indent n lines =
  let
    space = fold $ A.replicate n " "
  in
    space <> intercalate ("\n" <> space) lines

showPos :: Int -> String
showPos pos = "(Position " <> show pos <> ")"

listWords :: String -> Array String -> String
listWords sep ws = case A.init ws of
  Nothing -> ""
  Just [] -> last' ws
  Just i -> fold [ intercalate ", " i, " ", sep, " ", last' ws ]
  where
  last' xs = fromMaybe mempty $ A.last ws

-- | Always prefer later positions and custom errors when merging errors.
instance parseErrorSemigroup :: Semigroup ParseError where
  append = eAppend

eAppend :: ParseError -> ParseError -> ParseError
eAppend e1 e2 = case compare (ePos e1) (ePos e2) of
  LT -> e2
  EQ -> case e1, e2 of
    BasicError _, CustomError _ -> e2
    CustomError _, BasicError _ -> e1
    BasicError be1, BasicError be2 ->
      BasicError
        { expt: be1.expt `A.union` be2.expt
        , unexpt: be1.unexpt <|> be2.unexpt
        , pos: be1.pos
        }
    CustomError ce1, CustomError ce2 ->
      CustomError
        { msgs: ce1.msgs `A.union` ce2.msgs, pos: ce1.pos }
  GT -> e1

-- | Note: This is technically only a valid monoid instance if `pos` is
-- | unsigned.
instance parseErrorMonoid :: Monoid ParseError where
  mempty =
    BasicError
      { expt: []
      , unexpt: empty
      , pos: 0
      }

ePos :: ParseError -> Int
ePos (BasicError { pos }) = pos

ePos (CustomError { pos }) = pos
