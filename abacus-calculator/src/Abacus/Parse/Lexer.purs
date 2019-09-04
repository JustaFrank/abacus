module Abacus.Parse.Lexer
  ( lexeme
  ) where

import Prelude
import Abacus.Parse.Char (whitespace)
import Abacus.Parse.Parser (Parser)
import Data.List (many)

lexeme :: forall a. Parser a -> Parser a
lexeme p = p <* many whitespace
