module Text.Parse.ExprTokenSpec
  ( spec
  ) where

import Prelude

import Data.Array (many)
import Data.Either (Either(..), isLeft)
import Data.String (codePointFromChar)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Text.Parse.ExprToken (ExprToken(..), parseExprCloseParen, parseExprComma, parseExprFunc, parseExprLiteral, parseExprOpenParen, parseExprOper, parseExprGroup)
import Text.Parse.Parser (runParser)

spec :: Spec Unit
spec = do
  describe "Group Parser" do
    it "parses X-O-X" do
      let state = runParser parseExprGroup "1-3"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprLiteral 1.0
                                         , ExprOper $ codePointFromChar '-'
                                         , ExprLiteral 3.0
                                         ]
                                }
    it "parses X-O-X-O-X-O-X" do
      let state = runParser parseExprGroup "1+2-3^4"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprLiteral 1.0
                                         , ExprOper $ codePointFromChar '+'
                                         , ExprLiteral 2.0
                                         , ExprOper $ codePointFromChar '-'
                                         , ExprLiteral 3.0
                                         , ExprOper $ codePointFromChar '^'
                                         , ExprLiteral 4.0
                                         ]
                                }
    it "parses (X-O-X)" do
      let state = runParser parseExprGroup "(1-3)"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprOpenParen
                                         , ExprLiteral 1.0
                                         , ExprOper $ codePointFromChar '-'
                                         , ExprLiteral 3.0
                                         , ExprCloseParen
                                         ]
                                }
    it "parses (X-O-X)-O-X" do
      let state = runParser parseExprGroup "(1-3)^4"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprOpenParen
                                         , ExprLiteral 1.0
                                         , ExprOper $ codePointFromChar '-'
                                         , ExprLiteral 3.0
                                         , ExprCloseParen
                                         , ExprOper $ codePointFromChar '^'
                                         , ExprLiteral 4.0
                                         ]
                                }
    it "parses (T)-O-X" do
      let state = runParser parseExprGroup "(1)^4"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprOpenParen
                                         , ExprLiteral 1.0
                                         , ExprCloseParen
                                         , ExprOper $ codePointFromChar '^'
                                         , ExprLiteral 4.0
                                         ]
                                }
    it "parses f(T)" do
      let state = runParser parseExprGroup "func(1)"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprFunc "func"
                                         , ExprOpenParen
                                         , ExprLiteral 1.0
                                         , ExprCloseParen
                                         ]
                                }
    it "parses f(T, T)" do
      let state = runParser parseExprGroup "func(1, 3)"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprFunc "func"
                                         , ExprOpenParen
                                         , ExprLiteral 1.0
                                         , ExprComma
                                         , ExprLiteral 3.0
                                         , ExprCloseParen
                                         ]
                                }
    it "ignores whitespace" do
      let state = runParser parseExprGroup "1 - 3"
      state `shouldEqual` Right { rest: ""
                                , token: [ ExprLiteral 1.0
                                         , ExprOper $ codePointFromChar '-'
                                         , ExprLiteral 3.0
                                         ]
                                }

  describe "Literal Parser" do
    it "parses positive integers" do
      let state = runParser parseExprLiteral "1234rest"
      state `shouldEqual` Right { rest: "rest", token: ExprLiteral 1234.0 }
    it "parses negative integers" do
      let state = runParser parseExprLiteral "-1234rest"
      state `shouldEqual` Right { rest: "rest", token: ExprLiteral (-1234.0) }
    it "parses floats" do
      let state = runParser parseExprLiteral "-1234.234rest"
      state `shouldEqual` Right { rest: "rest"
                                , token: ExprLiteral (-1234.234)
                                }
    it "fails when letter" do
      let state = runParser parseExprLiteral "notanumber"
      state `shouldSatisfy` isLeft
    it "fails when special character" do
      let state = runParser parseExprLiteral "-notanumber"
      state `shouldSatisfy` isLeft

  describe "Operator Parser" do
    it "parses special characters" do
      let state = runParser (many parseExprOper) "+-*/^rest"
      state `shouldEqual` Right { rest: "rest"
                                , token: map (ExprOper <<< codePointFromChar)
                                    ['+', '-', '*', '/', '^']
                                }
    it "fails when number" do
      let state = runParser parseExprOper "1234notanoper"
      state `shouldSatisfy` isLeft
    it "fails when letter" do
      let state = runParser parseExprOper "notanoper"
      state `shouldSatisfy` isLeft

  describe "Function Parser" do
    it "parses words" do
      let state = runParser parseExprFunc "funcName"
      state `shouldEqual` Right { rest: "", token: ExprFunc "funcName" }
    it "parses numbers in function name" do
      let state = runParser parseExprFunc "f1"
      state `shouldEqual` Right { rest: "", token: ExprFunc "f1" }
    it "stops at special characters" do
      let state = runParser parseExprFunc "f1$"
      state `shouldEqual` Right { rest: "$", token: ExprFunc "f1" }
    it "stops at parentheses" do
      let state = runParser parseExprFunc "f1()"
      state `shouldEqual` Right { rest: "()", token: ExprFunc "f1" }
    it "fails when number at beginning" do
      let state = runParser parseExprOper "1notafunc"
      state `shouldSatisfy` isLeft

  describe "Parentheses and Comma Parser" do
    it "parses open parentheses" do
      let state = runParser parseExprOpenParen "(rest"
      state `shouldEqual` Right { rest: "rest", token: ExprOpenParen }
    it "parses close parentheses" do
      let state = runParser parseExprCloseParen ")rest"
      state `shouldEqual` Right { rest: "rest", token: ExprCloseParen }
    it "parses commas" do
      let state = runParser parseExprComma ",rest"
      state `shouldEqual` Right { rest: "rest", token: ExprComma }
    it "fails when nothing" do
      let state = runParser parseExprOpenParen "notaparen"
      state `shouldSatisfy` isLeft
