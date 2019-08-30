module Main (test) where

import Prelude

import Data.Either (Either)
import Data.List (List)
import Data.String (CodePoint, codePointFromChar)
import Effect (Effect)
import Effect.Console (log)

import Text.Parse.Base (char)
import Text.Parse.Parser (Parser(..))
import Text.Parse.State (State)

main :: Effect Unit
main = do
  log "Hello sailor!"

test :: Char -> String -> Either (List String) (State CodePoint)
test c xs = let Parser runParser = char $ codePointFromChar c
            in  runParser xs
