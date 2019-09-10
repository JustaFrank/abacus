module Test.Abacus.Expr.Parse (spec) where

import Prelude
import Abacus.Expr.Parse (ExprParser, parseCommaGroup, parseDeclaration, parseExpr, parseExprGroup, parseFuncGroup, parseParenGroup, parseProduct, parseTerm)
import Abacus.Expr.Token (ExprEnv, ExprToken(..), Var(..))
import Abacus.Expr.Token.Default as Default
import Abacus.Parse (ParseResponse, Parser, runParser)
import Control.Monad.Reader (runReaderT)
import Data.String (codePointFromChar)
import Test.Abacus.SpecUtils (shouldFailOn, shouldParse)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "Test.Abacus.Expr.Parse" do
    testParseExpr
    testParseExprGroup
    testParseTerm
    testParseDeclaration
    testParseProduct
    testParseParenGroup
    testParseFuncGroup
    testParseCommaGroup

testParseExpr :: Spec Unit
testParseExpr =
  describe "parseExpr" do
    it "allows leading whitespace" do
      run parseExpr " 1" `shouldParse` [ oneT ]
    it "fails when error before eof" do
      toParser parseExpr `shouldFailOn` "1 + 2 +"

testParseExprGroup :: Spec Unit
testParseExprGroup =
  describe "parseExprGroup" do
    it "parses terms separated by operators" do
      run parseExprGroup "1 + 2 + 3"
        `shouldParse`
          [ oneT, addT, twoT, addT, threeT ]
    it "parses a single term" do
      run parseExprGroup "1" `shouldParse` [ oneT ]
    it "parses an implicit product" do
      run parseExprGroup "1 (2)"
        `shouldParse`
          [ oneT, multT, openT, twoT, closeT ]
    it "parses double subtraction into term minus negative number" do
      run parseExprGroup "1--2"
        `shouldParse`
          [ oneT, subT, negTwoT ]
    it "stops at extra operator" do
      run parseExprGroup "1++2"
        `shouldParse`
          [ oneT ]

testParseTerm :: Spec Unit
testParseTerm =
  describe "parseTerm" do
    it "parses numbers" do
      run parseTerm "1" `shouldParse` [ oneT ]
    it "parses variables" do
      run parseTerm "a" `shouldParse` [ oneT ]
    it "parses parentheses groups" do
      run parseTerm "(1)" `shouldParse` [ openT, oneT, closeT ]
    it "parses function groups" do
      run parseTerm "sin(1)" `shouldParse` [ sinT, openT, oneT, closeT ]
    it "parses only the first term" do
      run parseTerm "1 * 2" `shouldParse` [ oneT ]
    it "fails on operator" do
      toParser parseTerm `shouldFailOn` "+"

testParseDeclaration :: Spec Unit
testParseDeclaration =
  describe "parseDeclaration" do
    it "parses symbol declarations" do
      run parseDeclaration "a = 1" `shouldParse` [ aT, equalsT, oneT ]
    it "fails when word" do
      toParser parseDeclaration `shouldFailOn` "abc = 1"
    it "fails when no equals" do
      toParser parseDeclaration `shouldFailOn` "a 1"
    it "fails when no expression" do
      toParser parseDeclaration `shouldFailOn` "a ="

testParseProduct :: Spec Unit
testParseProduct =
  describe "parseProduct" do
    it "multiplies format: (..) (..)" do
      run parseProduct "(1) (2)"
        `shouldParse`
          [ openT, oneT, closeT, multT, openT, twoT, closeT ]
    it "multiplies format: n (..)" do
      run parseProduct "1 (2)"
        `shouldParse`
          [ oneT, multT, openT, twoT, closeT ]
    it "multiplies format: (..) n" do
      run parseProduct "(1 ) 2"
        `shouldParse`
          [ openT, oneT, closeT, multT, twoT ]
    it "multiplies format: v (..)" do
      run parseProduct "a (2)"
        `shouldParse`
          [ oneT, multT, openT, twoT, closeT ]
    it "multiplies format: (..) v" do
      run parseProduct "(1) b"
        `shouldParse`
          [ openT, oneT, closeT, multT, twoT ]
    it "multiplies format: v v" do
      run parseProduct "a b"
        `shouldParse`
          [ oneT, multT, twoT ]
    it "multiplies more than 2 terms" do
      run parseProduct "a b c (1)"
        `shouldParse`
          [ oneT, multT, twoT, multT, threeT, multT, openT, oneT, closeT ]
    it "multiplies more than 2 terms" do
      run parseProduct "a 2 c (1)"
        `shouldParse`
          [ oneT, multT, twoT, multT, threeT, multT, openT, oneT, closeT ]
    it "fails on two literals in a row" do
      toParser parseProduct `shouldFailOn` "a 2 3"
    it "fails on single term" do
      toParser parseProduct `shouldFailOn` "1"

testParseParenGroup :: Spec Unit
testParseParenGroup =
  describe "parseParenGroup" do
    it "parses parentheses groups" do
      run parseParenGroup "(1 + 2 * 3)"
        `shouldParse`
          [ openT, oneT, addT, twoT, multT, threeT, closeT ]
    it "parses single number in parentheses" do
      run parseParenGroup "(1)" `shouldParse` [ openT, oneT, closeT ]
    it "fails on commas" do
      toParser parseParenGroup `shouldFailOn` "(1, 2)"
    it "fails when missing close parentheses" do
      toParser parseParenGroup `shouldFailOn` "(1 + 2"

testParseFuncGroup :: Spec Unit
testParseFuncGroup =
  describe "parseFuncGroup" do
    it "parses function groups" do
      run parseFuncGroup "sin(1)"
        `shouldParse`
          [ sinT, openT, oneT, closeT ]
    it "fails when no word" do
      toParser parseFuncGroup `shouldFailOn` "(1)"
    it "fails when word has no binding" do
      toParser parseFuncGroup `shouldFailOn` "abc(1)"

testParseCommaGroup :: Spec Unit
testParseCommaGroup =
  describe "parseCommaGroup" do
    it "parses comma groups" do
      run parseCommaGroup "(1, 2, 3)"
        `shouldParse`
          [ openT, oneT, commaT, twoT, commaT, threeT, closeT ]
    it "parses comma groups with single token" do
      run parseCommaGroup "(1)"
        `shouldParse`
          [ openT, oneT, closeT ]
    it "parses expressions" do
      run parseCommaGroup "(1 + 2, 3)"
        `shouldParse`
          [ openT, oneT, addT, twoT, commaT, threeT, closeT ]
    it "fails when no close parentheses" do
      toParser parseCommaGroup `shouldFailOn` "(1, 2"
    it "fails when extra comma" do
      toParser parseCommaGroup `shouldFailOn` "(1,, 2)"

run :: forall a. ExprParser a -> String -> ParseResponse a
run p = runParser $ toParser p

toParser :: forall a. ExprParser a -> Parser a
toParser p = runReaderT p defaultEnv

defaultEnv :: ExprEnv
defaultEnv =
  { funcs: Default.funcs
  , opers: Default.opers
  , vars:
    [ Var { symbol: codePointFromChar 'a', val: 1.0 }
    , Var { symbol: codePointFromChar 'b', val: 2.0 }
    , Var { symbol: codePointFromChar 'c', val: 3.0 }
    ]
  }

-- | The below values are used to make the above tests more concise.
oneT :: ExprToken
oneT = ExprLiteral 1.0

twoT :: ExprToken
twoT = ExprLiteral 2.0

threeT :: ExprToken
threeT = ExprLiteral 3.0

negOneT :: ExprToken
negOneT = ExprLiteral (-1.0)

negTwoT :: ExprToken
negTwoT = ExprLiteral (-2.0)

negThreeT :: ExprToken
negThreeT = ExprLiteral (-3.0)

addT :: ExprToken
addT = ExprOper Default.addO

subT :: ExprToken
subT = ExprOper Default.subO

multT :: ExprToken
multT = ExprOper Default.multO

divT :: ExprToken
divT = ExprOper Default.divO

expT :: ExprToken
expT = ExprOper Default.expO

equalsT :: ExprToken
equalsT = ExprOper Default.equals

sinT :: ExprToken
sinT = ExprFunc Default.sinF

openT :: ExprToken
openT = ExprOpenParen

closeT :: ExprToken
closeT = ExprCloseParen

commaT :: ExprToken
commaT = ExprComma

aT :: ExprToken
aT = ExprSymb $ codePointFromChar 'a'
