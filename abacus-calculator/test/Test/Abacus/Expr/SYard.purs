module Test.Abacus.Expr.SYard
  ( spec
  ) where

import Prelude
import Abacus.Expr.SYard (SYardState, criteria, nextSYardState, sYard)
import Abacus.Expr.Token (ExprToken(..), Oper(..), OperAssoc(..))
import Abacus.Expr.Token.Default as Default
import Control.Monad.State (runStateT)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.Tuple (snd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils.Token as T

spec :: Spec Unit
spec =
  describe "Abacus.Expr.SYard" do
    testSYard
    testNextSYardS
    testCriteria

testSYard :: Spec Unit
testSYard =
  describe "sYard" do
    it "converts to RPN" do
      sYard [ T.one, T.add, T.two ] `shouldEqual` Just [ T.one, T.two, T.add ]

testNextSYardS :: Spec Unit
testNextSYardS =
  describe "nextSYardS" do
    it "pushes numbers to output" do
      sndSYardS T.one `shouldEqual` Just { opers: [], output: [ T.one ] }
    it "pushes functions to operator stack" do
      sndSYardS T.sin `shouldEqual` Just { opers: [ T.sin ], output: [] }
    it "pushes operators to operator stack" do
      sndSYardS T.add `shouldEqual` Just { opers: [ T.add ], output: [] }
    it "pops operators until fail criteria when operator" do
      toNextSYardS T.mult
        { opers: [ T.mult, T.div, T.add ], output: [] }
        `shouldEqual`
          Just { opers: [ T.mult, T.add ], output: [ T.div, T.mult ] }
    it "pushes open parentheses to operator stack" do
      sndSYardS T.open
        `shouldEqual`
          Just { opers: [ T.open ], output: [] }
    it "pops operators until close parentheses when open parentheses" do
      toNextSYardS T.close
        { opers: [ T.add, T.mult, T.open, T.sub ], output: [] }
        `shouldEqual`
          Just { opers: [ T.sub ], output: [ T.mult, T.add ] }
    it "fails on close parentheses when no open parentheses" do
      sndSYardS T.close `shouldEqual` Nothing

sndSYardS :: ExprToken -> Maybe SYardState
sndSYardS t = toNextSYardS t { output: [], opers: [] }

toNextSYardS :: ExprToken -> SYardState -> Maybe SYardState
toNextSYardS t initS = snd <$> runStateT (nextSYardState t) initS

testCriteria :: Spec Unit
testCriteria =
  describe "criteria" do
    it "allows functions" do
      criteria Default.addO T.sin `shouldEqual` true
    it "denies left assoc operators with lower precedence" do
      criteria Default.multO T.add `shouldEqual` false
    it "allows left assoc operators with equal precedence" do
      criteria Default.addO T.sub `shouldEqual` true
    it "allows left assoc operators with greater precedence" do
      criteria Default.addO T.mult `shouldEqual` true
    it "denies right assoc operators with lower precedence" do
      criteria Default.addO (ExprOper testOperR) `shouldEqual` false
    it "denies right assoc operators with equal precedence" do
      criteria Default.expO T.exp `shouldEqual` false
    it "allows right assoc operators with greater precedence" do
      criteria Default.addO T.exp `shouldEqual` true
    it "denies open parentheses" do
      criteria Default.addO T.open `shouldEqual` false

testOperR :: Oper
testOperR =
  Oper
    { assoc: RightAssoc
    , preced: 2
    , symbol: codePointFromChar '$'
    , comp: { arity: 1, exec: \_ -> pure 0.0 }
    }
