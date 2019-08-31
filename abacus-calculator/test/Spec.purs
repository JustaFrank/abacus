module Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parse.ExprTokenSpec as Text.Parse.ExprTokenSpec

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Text.Parse.ExprTokenSpec" Text.Parse.ExprTokenSpec.spec
