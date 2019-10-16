module Abacus.Expr.Token.Default
  ( absF
  , addO
  , cosF
  , divO
  , equals
  , expO
  , funcs
  , maxF
  , minF
  , multO
  , opers
  , sinF
  , subO
  , tanF
  ) where

import Prelude
import Abacus.Expr.Token
  ( ExprEnv
  , ExprToken(..)
  , Func(..)
  , Oper(..)
  , OperAssoc(..)
  , TokenStack
  , Var(..)
  )
import Control.Apply (lift2)
import Control.Monad.State (StateT, lift, modify_)
import Data.Array ((!!), (:))
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Math as Math

equals :: Oper
equals =
  Oper
    { symbol: codePointFromChar '='
    , assoc: LeftAssoc
    , preced: 0
    , comp: { arity: 2, exec: execEquals }
    }

execEquals :: TokenStack -> StateT ExprEnv Maybe Number
execEquals ts = case ts of
  [ ExprSymb c, ExprLiteral n ] -> modify_ (bindVar c n) *> pure n
  _ -> lift Nothing
  where
  bindVar c n env = env { vars = Var { symbol: c, val: n } : env.vars }

opers :: Array Oper
opers = [ addO, subO, multO, divO, expO ]

addO :: Oper
addO = consOper '+' 2 LeftAssoc (+)

subO :: Oper
subO = consOper '-' 2 LeftAssoc (-)

multO :: Oper
multO = consOper '*' 3 LeftAssoc (*)

divO :: Oper
divO = consOper '/' 3 LeftAssoc (/)

expO :: Oper
expO = consOper '^' 4 RightAssoc (Math.pow)

-- Removed all functions from default!
funcs :: Array Func
funcs = []

sinF :: Func
sinF = consFunc1 "sin" Math.sin

cosF :: Func
cosF = consFunc1 "cos" Math.cos

tanF :: Func
tanF = consFunc1 "tan" Math.tan

absF :: Func
absF = consFunc1 "abs" Math.abs

minF :: Func
minF = consFunc2 "min" Math.min

maxF :: Func
maxF = consFunc2 "max" Math.max

consOper :: Char -> Int -> OperAssoc -> (Number -> Number -> Number) -> Oper
consOper c preced assoc f =
  Oper
    { assoc
    , preced
    , symbol: codePointFromChar c
    , comp: { arity: 2, exec: toCompExec2 f }
    }

consFunc1 :: String -> (Number -> Number) -> Func
consFunc1 s f =
  Func
    { symbol: s
    , comp: { arity: 1, exec: toCompExec1 f }
    }

consFunc2 :: String -> (Number -> Number -> Number) -> Func
consFunc2 s f =
  Func
    { symbol: s
    , comp: { arity: 2, exec: toCompExec2 f }
    }

toCompExec1 :: (Number -> Number) -> TokenStack -> StateT ExprEnv Maybe Number
toCompExec1 f ts = lift $ f <$> getNthNumber ts 0

toCompExec2 ::
  (Number -> Number -> Number) -> TokenStack -> StateT ExprEnv Maybe Number
toCompExec2 f ts =
  lift
    $ (lift2 f `on` getNthNumber ts) 0 1

getNthNumber :: TokenStack -> Int -> Maybe Number
getNthNumber ts n = ts !! n >>= tok2number

tok2number :: ExprToken -> Maybe Number
tok2number t = case t of
  ExprLiteral n -> Just n
  _ -> Nothing
