module Main where

import Prelude

import Abacus.ExprToken (createExprGroupParser)
import Abacus.Parse.Defaults as Defaults
import Abacus.Parse.Parser (runParser)
import Abacus.Postfix (infix2postfix)
import Data.Either (note)
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log <<< show $
    runParser
      (createExprGroupParser Defaults.opers Defaults.funcs)
      "1 + 3 - (1 * 234) / 23409 ^3 - 4"
        >>= _.token
        >>> infix2postfix
        >>> note ["Error converting infix to postfix."]
        <#> map show
        >>> intercalate " "
