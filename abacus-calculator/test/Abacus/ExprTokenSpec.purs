module Abacus.ExprTokenSpec
  ( spec
  ) where

import Prelude
import Data.Array (many)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Abacus.Defaults as Defaults
import Abacus.ExprToken (ExprToken(..), Func(..), createExprFuncParser, createExprGroupParser, createExprOperParser, parseExprCloseParen, parseExprComma, parseExprLiteral, parseExprOpenParen)
import Abacus.Parse.Parser (Parser, runParser)

spec :: Spec Unit
spec = do
  describe "Group Parser" do
    it "parses X-O-X" do
      let
        state = runParser parseExprGroup "1-3"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprLiteral 1.0
              , ExprOper $ Defaults.osub
              , ExprLiteral 3.0
              ]
            }
    it "parses X-O-X-O-X-O-X" do
      let
        state = runParser parseExprGroup "1+2-3^4"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprLiteral 1.0
              , ExprOper $ Defaults.oadd
              , ExprLiteral 2.0
              , ExprOper $ Defaults.osub
              , ExprLiteral 3.0
              , ExprOper $ Defaults.oexp
              , ExprLiteral 4.0
              ]
            }
    it "parses (X-O-X)" do
      let
        state = runParser parseExprGroup "(1-3)"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprOpenParen
              , ExprLiteral 1.0
              , ExprOper $ Defaults.osub
              , ExprLiteral 3.0
              , ExprCloseParen
              ]
            }
    it "parses (X-O-X)-O-X" do
      let
        state = runParser parseExprGroup "(1-3)^4"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprOpenParen
              , ExprLiteral 1.0
              , ExprOper $ Defaults.osub
              , ExprLiteral 3.0
              , ExprCloseParen
              , ExprOper $ Defaults.oexp
              , ExprLiteral 4.0
              ]
            }
    it "parses (T)-O-X" do
      let
        state = runParser parseExprGroup "(1)^4"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprOpenParen
              , ExprLiteral 1.0
              , ExprCloseParen
              , ExprOper $ Defaults.oexp
              , ExprLiteral 4.0
              ]
            }
    it "parses f(T)" do
      let
        state = runParser parseExprGroup "sin(1)"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprFunc Defaults.fsin
              , ExprOpenParen
              , ExprLiteral 1.0
              , ExprCloseParen
              ]
            }
    it "parses f(T, T)" do
      let
        state = runParser parseExprGroup "max(1, 3)"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprFunc Defaults.fmax
              , ExprOpenParen
              , ExprLiteral 1.0
              , ExprComma
              , ExprLiteral 3.0
              , ExprCloseParen
              ]
            }
    it "ignores whitespace" do
      let
        state = runParser parseExprGroup "1 - 3"
      state
        `shouldEqual`
          Right
            { rest: ""
            , token:
              [ ExprLiteral 1.0
              , ExprOper $ Defaults.osub
              , ExprLiteral 3.0
              ]
            }
  describe "Literal Parser" do
    it "parses positive integers" do
      let
        state = runParser parseExprLiteral "1234rest"
      state `shouldEqual` Right { rest: "rest", token: ExprLiteral 1234.0 }
    it "parses negative integers" do
      let
        state = runParser parseExprLiteral "-1234rest"
      state `shouldEqual` Right { rest: "rest", token: ExprLiteral (-1234.0) }
    it "parses floats" do
      let
        state = runParser parseExprLiteral "-1234.234rest"
      state
        `shouldEqual`
          Right
            { rest: "rest"
            , token: ExprLiteral (-1234.234)
            }
    it "fails when letter" do
      let
        state = runParser parseExprLiteral "notanumber"
      state `shouldSatisfy` isLeft
    it "fails when special character" do
      let
        state = runParser parseExprLiteral "-notanumber"
      state `shouldSatisfy` isLeft
  describe "Operator Parser" do
    it "parses special characters" do
      let
        state = runParser (many parseExprOper) "+-*/^rest"
      state
        `shouldEqual`
          Right
            { rest: "rest"
            , token:
              [ ExprOper $ Defaults.oadd
              , ExprOper $ Defaults.osub
              , ExprOper $ Defaults.omult
              , ExprOper $ Defaults.odiv
              , ExprOper $ Defaults.oexp
              ]
            }
    it "fails when number" do
      let
        state = runParser parseExprOper "1234notanoper"
      state `shouldSatisfy` isLeft
    it "fails when letter" do
      let
        state = runParser parseExprOper "notanoper"
      state `shouldSatisfy` isLeft
  describe "Function Parser" do
    it "parses words" do
      let
        state = runParser parseExprFunc "sin"
      state `shouldEqual` Right { rest: "", token: ExprFunc Defaults.fsin }
    it "parses numbers after letter" do
      let
        state = runParser parseExprFunc "hasNumbers123"
      state `shouldEqual` Right { rest: "", token: ExprFunc hasNumbers }
    it "stops at special characters" do
      let
        state = runParser parseExprFunc "sin$"
      state `shouldEqual` Right { rest: "$", token: ExprFunc Defaults.fsin }
    it "stops at parentheses" do
      let
        state = runParser parseExprFunc "sin()"
      state `shouldEqual` Right { rest: "()", token: ExprFunc Defaults.fsin }
    it "fails when number at beginning" do
      let
        state = runParser parseExprOper "1notafunc"
      state `shouldSatisfy` isLeft
  describe "Parentheses and Comma Parser" do
    it "parses open parentheses" do
      let
        state = runParser parseExprOpenParen "(rest"
      state `shouldEqual` Right { rest: "rest", token: ExprOpenParen }
    it "parses close parentheses" do
      let
        state = runParser parseExprCloseParen ")rest"
      state `shouldEqual` Right { rest: "rest", token: ExprCloseParen }
    it "parses commas" do
      let
        state = runParser parseExprComma ",rest"
      state `shouldEqual` Right { rest: "rest", token: ExprComma }
    it "fails when nothing" do
      let
        state = runParser parseExprOpenParen "notaparen"
      state `shouldSatisfy` isLeft

parseExprOper :: Parser ExprToken
parseExprOper = createExprOperParser Defaults.opers

parseExprFunc :: Parser ExprToken
parseExprFunc = createExprFuncParser testFuncs

parseExprGroup :: Parser (Array ExprToken)
parseExprGroup = createExprGroupParser Defaults.opers Defaults.funcs

testFuncs :: Array Func
testFuncs = Defaults.funcs <> [ hasNumbers ]

hasNumbers :: Func
hasNumbers =
  Func
    { symbol: "hasNumbers123"
    , arity: 1
    , exec: \_ -> Just 0.0
    }
