module Test.Abacus.Expr.Parse.Token
  ( spec
  ) where

import Prelude
import Abacus.Expr.Parse.Token
  ( exprCloseParen
  , exprComma
  , exprEq
  , exprFunc
  , exprLiteral
  , exprOpenParen
  , exprOper
  , exprSymb
  , exprVar
  )
import Abacus.Expr.Token (ExprToken(..), Var(..))
import Abacus.Expr.Token.Default (equals)
import Abacus.Expr.Token.Default as Default
import Abacus.Parse (Parser, runParser)
import Control.Monad.Reader (runReaderT)
import Data.String (codePointFromChar)
import Test.Abacus.SpecUtils (shouldFailOn, shouldParse)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Abacus.Expr.Parse.Token" do
    testExprOper
    testExprFunc
    testExprVar
    testExprLiteral
    testExprEq
    testExprSymb
    testExprOpenParen
    testExprCloseParen
    testExprComma

testExprOper :: Spec Unit
testExprOper =
  describe "exprOper" do
    it "parses an operator from a special character" do
      runParser exprOper' "+" `shouldParse` ExprOper Default.addO
    it "fails when no word" do
      exprOper' `shouldFailOn` "123"
    it "fails when special character has no binding" do
      exprOper' `shouldFailOn` "$"

exprOper' :: Parser ExprToken
exprOper' = runReaderT exprOper Default.opers

testExprFunc :: Spec Unit
testExprFunc =
  describe "exprFunc" do
    it "parses a function from a word" do
      runParser exprFunc' "sin" `shouldParse` ExprFunc Default.sinF
    it "fails when no word" do
      exprFunc' `shouldFailOn` "123"
    it "fails when word has no binding" do
      exprFunc' `shouldFailOn` "abc"

exprFunc' :: Parser ExprToken
exprFunc' = runReaderT exprFunc Default.funcs

testExprVar :: Spec Unit
testExprVar =
  describe "exprVar" do
    it "parses a value from a single letter" do
      runParser exprVar' "abc" `shouldParse` ExprLiteral 1.0
    it "fails when no letter" do
      exprVar' `shouldFailOn` "123"
    it "fails when letter has no binding" do
      exprVar' `shouldFailOn` "xyz"

exprVar' :: Parser ExprToken
exprVar' =
  runReaderT exprVar
    [ Var { symbol: codePointFromChar 'a', val: 1.0 }
    , Var { symbol: codePointFromChar 'b', val: 2.0 }
    , Var { symbol: codePointFromChar 'c', val: 3.0 }
    ]

testExprLiteral :: Spec Unit
testExprLiteral =
  describe "exprLiteral" do
    it "parses integers" do
      runParser exprLiteral "123" `shouldParse` ExprLiteral 123.0
    it "parses floats" do
      runParser exprLiteral "123.456" `shouldParse` ExprLiteral 123.456
    it "parses floats without leading 0" do
      runParser exprLiteral ".123" `shouldParse` ExprLiteral 0.123
    it "stops at whitespace between numbers" do
      runParser exprLiteral "123 456" `shouldParse` ExprLiteral 123.0
    it "stops at second decimal point" do
      runParser exprLiteral "123.456.789" `shouldParse` ExprLiteral 123.456
    it "fails when no number" do
      exprLiteral `shouldFailOn` "abc123"

testExprEq :: Spec Unit
testExprEq =
  describe "exprEq" do
    it "parses equals sign" do
      runParser exprEq "=" `shouldParse` ExprOper equals
    it "fails when no equals sign" do
      exprEq `shouldFailOn` "123"

testExprSymb :: Spec Unit
testExprSymb =
  describe "exprSymb" do
    it "parses a single letter" do
      runParser exprSymb "abc" `shouldParse` ExprSymb (codePointFromChar 'a')
    it "fails when no letter" do
      exprSymb `shouldFailOn` "123"

testExprOpenParen :: Spec Unit
testExprOpenParen =
  describe "exprOpenParen" do
    it "parses an open parentheses" do
      runParser exprOpenParen "(" `shouldParse` ExprOpenParen
    it "fails when no open parentheses" do
      exprOpenParen `shouldFailOn` "123"

testExprCloseParen :: Spec Unit
testExprCloseParen =
  describe "exprCloseParen" do
    it "parses a close parentheses" do
      runParser exprCloseParen ")" `shouldParse` ExprCloseParen
    it "fails when no close parentheses" do
      exprCloseParen `shouldFailOn` "123"

testExprComma :: Spec Unit
testExprComma =
  describe "exprComma" do
    it "parses a comma" do
      runParser exprComma "," `shouldParse` ExprComma
    it "fails when no comma" do
      exprComma `shouldFailOn` "123"
