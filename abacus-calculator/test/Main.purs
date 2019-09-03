module Test.Main where

import Prelude
import Abacus.ExprTokenSpec as Abacus.ExprTokenSpec
import Abacus.PostfixSpec as Abacus.PostfixSpec
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Abacus.ExprTokenSpec" Abacus.ExprTokenSpec.spec
        describe "Abacus.PostfixSpec" Abacus.PostfixSpec.spec
