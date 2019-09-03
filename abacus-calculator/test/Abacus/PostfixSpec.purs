module Abacus.PostfixSpec
  ( spec
  ) where

import Prelude
import Abacus.Defaults as Defaults
import Abacus.ExprToken (ExprToken(..), Func(..), Oper(..), OperAssoc(..))
import Abacus.Postfix (criteria, execFunc, getLiteral, nextPEvalS, nextSYardS, popUntilParen, pushOper)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Shunting Yard" do
    describe "nextSYardS" do
      it "pushes literals to output" do
        let
          tok = ExprLiteral 3.0

          s = nextSYardS { opers: [], output: [] } tok
        s `shouldEqual` Just { opers: [], output: [ tok ] }
      it "pushes functions to opers" do
        let
          tok = ExprFunc Defaults.fsin

          s = nextSYardS { opers: [], output: [] } tok
        s `shouldEqual` Just { opers: [ tok ], output: [] }
      it "pushes open parentheses to opers" do
        let
          tok = ExprOpenParen

          s = nextSYardS { opers: [], output: [] } tok
        s `shouldEqual` Just { opers: [ tok ], output: [] }
      it "ignores commas" do
        let
          tok = ExprComma

          s = nextSYardS { opers: [], output: [] } tok
        s `shouldEqual` Just { opers: [], output: [] }
    describe "criteria" do
      it "allows functions" do
        let
          oper = Defaults.oadd

          tok = ExprFunc Defaults.fsin

          rslt = criteria oper tok
        rslt `shouldEqual` true
      it "allows right assoc opers w/ greater preced" do
        let
          oper = Defaults.oadd

          tok = ExprOper Defaults.oexp

          rslt = criteria oper tok
        rslt `shouldEqual` true
      it "does not allow right assoc opers w/ equal preced" do
        let
          oper = Defaults.oadd

          tok = ExprOper oright

          rslt = criteria oper tok
        rslt `shouldEqual` false
      it "does not allow right assoc opers w/ lower preced" do
        let
          oper = Defaults.omult

          tok = ExprOper oright

          rslt = criteria oper tok
        rslt `shouldEqual` false
      it "allows left assoc opers w/ equal preced" do
        let
          oper = Defaults.oadd

          tok = ExprOper Defaults.osub

          rslt = criteria oper tok
        rslt `shouldEqual` true
      it "does not allow left assoc opers w/ lower preced" do
        let
          oper = Defaults.omult

          tok = ExprOper Defaults.oadd

          rslt = criteria oper tok
        rslt `shouldEqual` false
      it "does not allow open parentheses" do
        let
          oper = Defaults.oadd

          tok = ExprOpenParen

          rslt = criteria oper tok
        rslt `shouldEqual` false
    describe "pushOper" do
      it "pushes when empty oper stack" do
        let
          oper = Defaults.oadd

          s = pushOper { opers: [], output: [] } oper
        s `shouldEqual` Just { opers: [ ExprOper oper ], output: [] }
      it "stops at left parentheses" do
        let
          oper = Defaults.oadd

          s = pushOper { opers: [ ExprOpenParen ], output: [] } oper
        s
          `shouldEqual`
            Just
              { opers: [ ExprOper oper, ExprOpenParen ]
              , output: []
              }
      it "moves opers over to output" do
        let
          oper = Defaults.oadd

          s =
            pushOper
              { opers:
                [ ExprOper Defaults.osub
                , ExprOper Defaults.omult
                , ExprOper Defaults.odiv
                , ExprOpenParen
                ]
              , output: []
              }
              oper
        s
          `shouldEqual`
            Just
              { opers: [ ExprOper oper, ExprOpenParen ]
              , output:
                [ ExprOper Defaults.odiv
                , ExprOper Defaults.omult
                , ExprOper Defaults.osub
                ]
              }
    describe "popUntilParen" do
      it "fails when empty oper stack" do
        let
          s = popUntilParen { opers: [], output: [] }
        s `shouldEqual` Nothing
      it "moves opers over to output" do
        let
          s =
            popUntilParen
              { opers:
                [ ExprOper Defaults.osub
                , ExprOper Defaults.omult
                , ExprOper Defaults.odiv
                , ExprOpenParen
                ]
              , output: []
              }
        s
          `shouldEqual`
            Just
              { opers: []
              , output:
                [ ExprOper Defaults.odiv
                , ExprOper Defaults.omult
                , ExprOper Defaults.osub
                ]
              }
  describe "Postfix Evaluator" do
    describe "getLiteral" do
      it "gets a literal" do
        let
          x = getLiteral (ExprLiteral 3.0)
        x `shouldEqual` Just 3.0
      it "fails when different kind of token" do
        let
          x = getLiteral ExprComma
        x `shouldEqual` Nothing
    describe "execFunc" do
      it "executes a unary function" do
        let
          Func { exec } = Defaults.fsin

          stack = [ ExprLiteral 1.0 ]

          stack' = execFunc exec 1 stack
        stack' `shouldEqual` Just [ ExprLiteral 0.8414709848078965 ]
      it "executes a binary function" do
        let
          Oper { exec } = Defaults.oadd

          stack = [ ExprLiteral (-23.0), ExprLiteral 43.0 ]

          stack' = execFunc exec 2 stack
        stack' `shouldEqual` Just [ ExprLiteral 20.0 ]
      it "fails when incorrect arity given" do
        let
          Oper { exec } = Defaults.oadd

          stack = [ ExprLiteral (-23.0), ExprLiteral 43.0 ]

          stack' = execFunc exec 1 stack
        stack' `shouldEqual` Nothing
      it "fails when not enough tokens" do
        let
          Oper { exec } = Defaults.oadd

          stack = [ ExprLiteral (-23.0) ]

          stack' = execFunc exec 2 stack
        stack' `shouldEqual` Nothing
    describe "nextPEvalS" do
      it "pushes literals to the stack" do
        let
          tok = ExprLiteral 3.0

          stack = [ ExprComma ]

          stack' = nextPEvalS stack tok
        stack' `shouldEqual` Just [ ExprLiteral 3.0, ExprComma ]
      it "evaluates from stack when operator" do
        let
          tok = ExprOper Defaults.oadd

          stack = [ ExprLiteral 3.0, ExprLiteral 1.0, ExprLiteral 7.0 ]

          stack' = nextPEvalS stack tok
        stack' `shouldEqual` Just [ ExprLiteral 4.0, ExprLiteral 7.0 ]
      it "evaluates from stack when function" do
        let
          tok = ExprFunc Defaults.fmin

          stack = [ ExprLiteral 3.0, ExprLiteral 1.0, ExprLiteral 7.0 ]

          stack' = nextPEvalS stack tok
        stack' `shouldEqual` Just [ ExprLiteral 1.0, ExprLiteral 7.0 ]

-- Used for testing criteria
oright :: Oper
oright =
  Oper
    { assoc: RightAssoc
    , preced: 1
    , symbol: codePointFromChar '%'
    , exec: \_ -> Just 0.0
    }
