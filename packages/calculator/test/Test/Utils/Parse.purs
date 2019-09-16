module Test.Utils.Parse
  ( shouldParse
  , shouldFailOn
  ) where

import Prelude
import Abacus.Parse (ParseResponse, Parser, runParser)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..), isLeft)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

shouldParse ::
  forall m a.
  MonadThrow Error m ⇒ Show a ⇒ Eq a ⇒ ParseResponse a → a → m Unit
shouldParse r v = case r of
  Left _ -> fail $ show r <> "≠" <> show v
  Right { result } -> result `shouldEqual` v

shouldFailOn ::
  forall m a.
  MonadThrow Error m ⇒ Show a => Parser a → String → m Unit
shouldFailOn p s = runParser p s `shouldSatisfy` isLeft
