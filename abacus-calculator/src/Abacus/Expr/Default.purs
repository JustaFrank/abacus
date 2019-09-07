module Abacus.Expr.Default
  ( funcs
  , mult
  , opers
  ) where

import Prelude
import Abacus.Expr.Token
  ( ExprEnv
  , ExprToken(..)
  , Func(..)
  , Oper(..)
  , OperAssoc(..)
  , TokenStack
  )
import Control.Apply (lift2)
import Control.Monad.State (StateT, lift)
import Data.Array ((!!))
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Math as Math

mult :: Oper
mult = consOper '*' 3 LeftAssoc (*)

opers :: Array Oper
opers =
  [ consOper '+' 2 LeftAssoc (+)
  , consOper '-' 2 LeftAssoc (-)
  , consOper '*' 3 LeftAssoc (*)
  , consOper '/' 3 LeftAssoc (/)
  , consOper '^' 4 RightAssoc (Math.pow)
  ]

funcs :: Array Func
funcs =
  [ consFunc1 "sin" Math.sin
  , consFunc1 "cos" Math.cos
  , consFunc1 "tan" Math.tan
  , consFunc1 "abs" Math.abs
  , consFunc2 "min" Math.min
  , consFunc2 "max" Math.max
  ]

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
