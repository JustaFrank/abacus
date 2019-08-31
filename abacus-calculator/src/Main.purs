module Main (char, string, test) where

import Prelude

import Data.Either (Either)
import Data.String (codePointFromChar, fromCodePointArray, singleton, toCodePointArray)
import Effect (Effect)
import Effect.Console (log)
import Text.Parse.Base (codePoint, codePointArray)
import Text.Parse.ExprToken (ExprToken, parseExprLiteral)
import Text.Parse.Parser (Parser(..))
import Text.Parse.State (State)

main :: Effect Unit
main = do
  log "Hello sailor!"

char :: Char -> String -> Either (Array String) (State String)
char c xs = let Parser runParser = codePoint $ codePointFromChar c
            in  map toStrState $ runParser xs
 where toStrState state = state { token = singleton state.token }

string :: String -> String -> Either (Array String) (State String)
string xs ys = let Parser runParser = codePointArray $ toCodePointArray xs
               in  map toStrState $ runParser ys
 where toStrState state = state { token = fromCodePointArray state.token }

test :: String -> Either (Array String) (State ExprToken)
test = let Parser runParser = parseExprLiteral
       in  runParser