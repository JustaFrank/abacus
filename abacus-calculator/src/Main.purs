module Main where

import Prelude

import Abacus.ExprToken (Func(..), createExprGroupParser)
import Abacus.Parse.Defaults as Defaults
import Abacus.Parse.Parser (runParser)
import Abacus.Postfix (evalPostfix, infix2postfix)
import Data.Either (note)
import Data.Foldable (intercalate)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log <<< show $
    runParser
      (createExprGroupParser Defaults.opers Defaults.funcs)
      "min(1, 2)"
        >>= _.token
        >>> infix2postfix
        >>> note ["Error converting infix to postfix."]
        <#> map show
        >>> intercalate " "
  log <<< show $
    runParser
      (createExprGroupParser Defaults.opers Defaults.funcs)
      "pow(1, 2)"
        >>= _.token
        >>> infix2postfix
        >>> (_ >>= evalPostfix)
        >>> note ["Error!"]
        <#> show
  log <<< show $ let Func f = Defaults.fmin
                 in f.exec [1.0, 2.0]
