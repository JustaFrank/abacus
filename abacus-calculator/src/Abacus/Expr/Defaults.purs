module Abacus.Expr.Defaults where

import Prelude
import Data.Array ((!!))
import Data.Maybe (Maybe)
import Data.String (codePointFromChar)
import Math as Math
import Abacus.Expr.Token (Func(..), Oper(..), OperAssoc(..))

to1ArityFunc :: forall a. (a -> a) -> Array a -> Maybe a
to1ArityFunc f = \xs -> f <$> xs !! 0

to2ArityFunc :: forall a. (a -> a -> a) -> Array a -> Maybe a
to2ArityFunc f = \xs -> f <$> xs !! 0 <*> xs !! 1

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
