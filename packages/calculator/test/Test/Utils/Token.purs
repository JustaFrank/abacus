module Test.Utils.Token where

import Prelude
import Abacus.Expr.Token (ExprEnv, ExprToken(..), Var(..))
import Abacus.Expr.Token.Default as Default
import Data.String (codePointFromChar)

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

one :: ExprToken
one = ExprLiteral 1.0

two :: ExprToken
two = ExprLiteral 2.0

three :: ExprToken
three = ExprLiteral 3.0

negOne :: ExprToken
negOne = ExprLiteral (-1.0)

negTwo :: ExprToken
negTwo = ExprLiteral (-2.0)

negThree :: ExprToken
negThree = ExprLiteral (-3.0)

add :: ExprToken
add = ExprOper Default.addO

sub :: ExprToken
sub = ExprOper Default.subO

mult :: ExprToken
mult = ExprOper Default.multO

div :: ExprToken
div = ExprOper Default.divO

exp :: ExprToken
exp = ExprOper Default.expO

equals :: ExprToken
equals = ExprOper Default.equals

sin :: ExprToken
sin = ExprFunc Default.sinF

min :: ExprToken
min = ExprFunc Default.minF

open :: ExprToken
open = ExprOpenParen

close :: ExprToken
close = ExprCloseParen

comma :: ExprToken
comma = ExprComma

a :: ExprToken
a = ExprSymb $ codePointFromChar 'a'

b :: ExprToken
b = ExprSymb $ codePointFromChar 'b'

c :: ExprToken
c = ExprSymb $ codePointFromChar 'c'
