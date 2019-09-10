module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Abacus.Expr.Parse.Token as Test.Abacus.Expr.Parse.Token
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Test.Abacus.Expr.Parse.Token.spec
