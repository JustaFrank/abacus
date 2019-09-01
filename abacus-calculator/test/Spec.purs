module Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Abacus.ExprTokenSpec as Abacus.ExprTokenSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Abacus.ExprTokenSpec" Abacus.ExprTokenSpec.spec
