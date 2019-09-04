module Abacus.Parse.Base
  ( eof
  , satisfy
  ) where

import Prelude
import Abacus.Parse.Error (ParseError(..))
import Abacus.Parse.Parser (Parser(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as S

-- | Parses a code point meeting a condition.
satisfy :: (CodePoint -> Boolean) -> Parser CodePoint
satisfy pred =
  Parser
    $ \{ rem: s, pos } -> case S.uncons s of
        Nothing ->
          Left
            $ BasicError
                { pos, expt: [], unexpt: Just "EOF" }
        Just { head: x, tail: xs }
          | pred x -> Right { state: { rem: xs, pos: pos + 1 }, result: x }
          | otherwise ->
            Left
              $ BasicError
                  { pos, expt: [], unexpt: Just $ S.singleton x
                  }

-- | Parses the end of input.
eof :: Parser Unit
eof = Parser pEof'
  where
  pEof' state@{ pos, rem: s }
    | s == "" = Right $ { state, result: unit }
    | otherwise =
      Left
        $ BasicError { pos, expt: [ "end of input" ], unexpt: Just s }
