module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Abacus.Expr.Eval as Test.Abacus.Expr.Eval
import Test.Abacus.Expr.Parse as Test.Abacus.Expr.Parse
import Test.Abacus.Expr.Parse.Token as Test.Abacus.Expr.Parse.Token
import Test.Abacus.Expr.SYard as Test.Abacus.Expr.SYard
import Test.Abacus.Parse.Char as Test.Abacus.Parse.Char
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Test.Abacus.Expr.Parse.spec
        Test.Abacus.Expr.Parse.Token.spec
        Test.Abacus.Expr.SYard.spec
        Test.Abacus.Expr.Eval.spec
        Test.Abacus.Parse.Char.spec
