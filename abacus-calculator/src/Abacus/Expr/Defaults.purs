module Abacus.Expr.Defaults where

import Prelude
import Abacus.Expr.Token (ExecFunc, ExprToken(..), Func(..), Oper(..), OperAssoc(..))
import Control.Monad.State (StateT(..))
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar)
import Data.Tuple (Tuple(..))
import Math as Math

to1ArityFunc :: (Number -> Number) -> ExecFunc
to1ArityFunc f xs =
  StateT
    $ \env ->
        (\x -> Tuple x env) <<< f
          <$> (xs !! 0 >>= tok2number)

to2ArityFunc :: (Number -> Number -> Number) -> ExecFunc
to2ArityFunc f xs =
  StateT
    $ \env ->
        (\x -> Tuple x env)
          <$> ( f
                <$> (xs !! 0 >>= tok2number)
                <*> (xs !! 1 >>= tok2number)
            )

tok2number :: ExprToken -> Maybe Number
tok2number (ExprLiteral n) = Just n

tok2number _ = Nothing

-- | Default opers
opers :: Array Oper
opers = [ oadd, osub, omult, odiv, oexp ]

oadd :: Oper
oadd =
  Oper
    { symbol: codePointFromChar '+'
    , preced: 2
    , assoc: LeftAssoc
    , exec: to2ArityFunc (+)
    }

osub :: Oper
osub =
  Oper
    { symbol: codePointFromChar '-'
    , preced: 2
    , assoc: LeftAssoc
    , exec: to2ArityFunc (-)
    }

omult :: Oper
omult =
  Oper
    { symbol: codePointFromChar '*'
    , preced: 3
    , assoc: LeftAssoc
    , exec: to2ArityFunc (*)
    }

odiv :: Oper
odiv =
  Oper
    { symbol: codePointFromChar '/'
    , preced: 3
    , assoc: LeftAssoc
    , exec: to2ArityFunc (/)
    }

oexp :: Oper
oexp =
  Oper
    { symbol: codePointFromChar '^'
    , preced: 4
    , assoc: RightAssoc
    , exec: to2ArityFunc Math.pow
    }

-- | Default functions
funcs :: Array Func
funcs = [ fsin, fcos, ftan, fabs, fmin, fmax ]

fsin :: Func
fsin =
  Func
    { symbol: "sin"
    , arity: 1
    , exec: to1ArityFunc Math.sin
    }

fcos :: Func
fcos =
  Func
    { symbol: "cos"
    , arity: 1
    , exec: to1ArityFunc Math.cos
    }

ftan :: Func
ftan =
  Func
    { symbol: "tan"
    , arity: 1
    , exec: to1ArityFunc Math.tan
    }

fabs :: Func
fabs =
  Func
    { symbol: "abs"
    , arity: 1
    , exec: to1ArityFunc Math.abs
    }

fmin :: Func
fmin =
  Func
    { symbol: "min"
    , arity: 2
    , exec: to2ArityFunc Math.min
    }

fmax :: Func
fmax =
  Func
    { symbol: "max"
    , arity: 2
    , exec: to2ArityFunc Math.max
    }
