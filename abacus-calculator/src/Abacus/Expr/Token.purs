module Abacus.Expr.Token where

import Prelude
import Control.Monad.State (StateT(..), lift)
import Data.Array ((:))
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (CodePoint, codePointFromChar)
import Data.String as S
import Data.Tuple (Tuple(..))

-- | Equal sign is separate from operator list since it has different parsing
-- | rules.
eqOper :: Oper
eqOper =
  Oper
    { symbol: codePointFromChar '='
    , assoc: LeftAssoc
    , preced: 0
    , comp:
      { exec: eqExec
      , arity: 2
      }
    }

eqExec :: TokenStack -> StateT ExprEnv Maybe Number
eqExec [ ExprSymb c, ExprLiteral n ] =
  StateT
    $ \env@{ vars } ->
        Just $ Tuple n $ env { vars = Var { symbol: c, val: n } : vars }

eqExec _ = lift Nothing

-- | TypeT that stores the environment for the expression parser.
type ExprEnv
  = { opers :: Array Oper
    , funcs :: Array Func
    , vars :: Array Var
    }

-- | Expression token datatype. A token can be a literal (number), operator,
-- | function, parentheses, or comma.
data ExprToken
  = ExprLiteral Number
  | ExprOper Oper
  | ExprFunc Func
  | ExprSymb CodePoint
  | ExprOpenParen
  | ExprCloseParen
  | ExprComma

type TokenStack
  = Array ExprToken

derive instance eqExprToken :: Eq ExprToken

instance showExprToken :: Show ExprToken where
  show (ExprLiteral n) = show n
  show (ExprOper oper) = show oper
  show (ExprFunc func) = show func
  show (ExprSymb c) = S.singleton c
  show ExprOpenParen = "("
  show ExprCloseParen = ")"
  show ExprComma = ","

-- | Operator datatype.
newtype Oper
  = Oper
  { symbol :: CodePoint
  , preced :: Int
  , assoc :: OperAssoc
  , comp :: Computation
  }

derive instance operNewtype :: Newtype Oper _

-- | Operators are equal as long as everything except the `comp` is equal.
instance operEq :: Eq Oper where
  eq ( Oper { symbol: s, preced: p, assoc: a }
  ) (Oper { symbol: s', preced: p', assoc: a' }) =
    s == s'
      && p
      == p'
      && a
      == a'

instance operShow :: Show Oper where
  show (Oper { symbol }) = S.singleton symbol

-- | Type that represents operator associativity.
data OperAssoc
  = LeftAssoc
  | RightAssoc

derive instance operAssocEq :: Eq OperAssoc

derive instance operAssocGeneric :: Generic OperAssoc _

instance operAssocShow :: Show OperAssoc where
  show = genericShow

-- | Function datatype.
newtype Func
  = Func
  { symbol :: String
  , comp :: Computation
  }

derive instance funcNewtype :: Newtype Func _

-- | Functions are equal as long as everything except the `comp` is equal.
instance funcEq :: Eq Func where
  eq (Func { symbol: s }) (Func { symbol: s' }) = s == s'

instance funcShow :: Show Func where
  show (Func { symbol }) = symbol

-- | Variable datatype.
newtype Var
  = Var
  { symbol :: CodePoint
  , val :: Number
  }

derive instance varNewtype :: Newtype Var _

derive instance varEq :: Eq Var

instance varShow :: Show Var where
  show (Var { symbol }) = S.singleton symbol

-- | Computation datatype.
type Computation
  = { exec :: TokenStack -> StateT ExprEnv Maybe Number
    , arity :: Arity
    }

type Arity
  = Int

execComp :: Computation -> TokenStack -> StateT ExprEnv Maybe Number
execComp { exec, arity } ts
  | A.length ts == arity = exec ts
  | otherwise = lift Nothing
