module Test.Abacus.Expr.Eval
  ( spec
  ) where

import Prelude
import Abacus.Expr.Eval (EvalState, eval, execCompFromStack, nextEvalState)
import Abacus.Expr.Token (Computation, TokenStack)
import Abacus.Expr.Token.Default as Default
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (StateT, runStateT)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils.Token as T

spec :: Spec Unit
spec =
  describe "Abacus.Expr.Eval" do
    testEval
    testNextEvalState
    testExecCompFromStack

testEval :: Spec Unit
testEval =
  describe "eval" do
    it "evaluates RPN" do
      eval T.defaultEnv [ T.one, T.two, T.add ]
        `shouldEqual`
          Just { result: 3.0, env: T.defaultEnv }

testNextEvalState :: Spec Unit
testNextEvalState =
  describe "nextEvalState" do
    it "pushes numbers to the stack" do
      runEvalState (nextEvalState T.one) [] `containsStack` [ T.one ]
    it "pushes to top of stack" do
      runEvalState (nextEvalState T.one) [ T.three, T.two ]
        `containsStack`
          [ T.one, T.three, T.two ]
    it "pushes symbols to the stack" do
      runEvalState (nextEvalState T.a) [] `containsStack` [ T.a ]
    it "executes functions on stack" do
      runEvalState (nextEvalState T.min) [ T.one, T.three ]
        `containsStack`
          [ T.one ]
    it "executes operators on stack" do
      runEvalState (nextEvalState T.add) [ T.one, T.two ]
        `containsStack`
          [ T.three ]
    it "fails on invalid token type" do
      runEvalState (nextEvalState T.add) [ T.comma ] `shouldEqual` Nothing

testExecCompFromStack :: Spec Unit
testExecCompFromStack =
  describe "execCompFromStack" do
    it "executes top of stack with unary function" do
      runEvalState (execCompFromStack absC) [ T.negOne ]
        `containsStack`
          [ T.one ]
    it "executes top of stack with binary function" do
      runEvalState (execCompFromStack addC) [ T.one, T.two ]
        `containsStack`
          [ T.three ]
    it "executes arguments in order" do
      runEvalState (execCompFromStack subC) [ T.one, T.two ]
        `containsStack`
          [ T.one ]
    it "pushes result to top of stack" do
      runEvalState (execCompFromStack subC) [ T.one, T.two, T.three ]
        `containsStack`
          [ T.one, T.three ]
    it "fails when stack is too small" do
      runEvalState (execCompFromStack addC) [] `shouldEqual` Nothing
    it "fails on incorrect token type" do
      runEvalState (execCompFromStack addC) [ T.one, T.add ]
        `shouldEqual`
          Nothing

runEvalState ::
  forall a. StateT EvalState Maybe a -> TokenStack -> Maybe EvalState
runEvalState state stack =
  snd
    <$> runStateT state { stack, env: T.defaultEnv }

containsStack ::
  forall m. MonadThrow Error m ⇒ Maybe EvalState → TokenStack → m Unit
containsStack state stack = map _.stack state `shouldEqual` Just stack

addC :: Computation
addC = (unwrap Default.addO).comp

subC :: Computation
subC = (unwrap Default.subO).comp

absC :: Computation
absC = (unwrap Default.absF).comp
