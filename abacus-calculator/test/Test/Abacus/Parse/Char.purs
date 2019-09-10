module Test.Abacus.Parse.Char where

import Prelude
import Abacus.Parse (char, runParser)
import Data.Either (Either(..))
import Data.String (codePointFromChar)
import Data.String.CodeUnits as SCU
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

spec :: Spec Unit
spec =
  describe "Abacus.Parse.Char" do
    testChar

testChar :: Spec Unit
testChar =
  describe "char" do
    it "parses the same character as the input" do
      quickCheck \n -> case runParser (char n) (SCU.singleton n) of
        Left _ -> false
        Right { result } -> result == codePointFromChar n
