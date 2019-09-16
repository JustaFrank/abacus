module Test.Abacus.Parse.Base where

import Prelude
import Abacus.Parse (eof, runParser, satisfy)
import Data.Either (Either(..))
import Data.String (codePointFromChar)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.String.Unsafe as SU
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Test.Utils.Parse (shouldFailOn, shouldParse)

spec :: Spec Unit
spec =
  describe "Abacus.Parse.Base" do
    testEof
    testSatisfy

testEof :: Spec Unit
testEof =
  describe "eof" do
    it "parses eof" do
      runParser eof "" `shouldParse` unit
    it "fails when no eof" do
      eof `shouldFailOn` "abc"

testSatisfy :: Spec Unit
testSatisfy =
  describe "satisfy" do
    it "parses if condition is true" do
      quickCheck \pred c -> case runParser
          (satisfy (pred <<< SU.char <<< S.singleton))
          (SCU.singleton c) of
        Right _ -> pred c == true
        Left _ -> pred c == false
    it "parses the first character" do
      quickCheck \c -> case runParser
          (satisfy \_ -> true)
          (SCU.singleton c) of
        Right { result } -> result == codePointFromChar c
        Left _ -> false
