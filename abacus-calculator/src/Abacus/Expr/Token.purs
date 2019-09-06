module Abacus.Expr.Token where

import Prelude
import Control.Monad.State (StateT(..), lift)
import Data.Array ((:))
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
    , exec: execEq
    }

execEq :: ExecFunc
execEq [ ExprSymb c, ExprLiteral n ] =
  StateT
    $ \env@{ vars } ->
        Just $ Tuple n $ env { vars = Var { symbol: c, val: n } : vars }

execEq _ = lift Nothing

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

derive instance eqExprToken :: Eq ExprToken

instance showExprToken :: Show ExprToken where
  show (ExprLiteral n) = show n
  show (ExprOper oper) = show oper
  show (ExprFunc func) = show func
  show (ExprSymb c) = S.singleton c
  show ExprOpenParen = "("
  show ExprCloseParen = ")"
  show ExprComma = ","

-- | Type signature for functions. The array input accounts for multiple
-- | parameters.
type ExecFunc
  = Array ExprToken -> StateT ExprEnv Maybe Number

-- | Operator datatype.
newtype Oper
  = Oper
  { symbol :: CodePoint
  , preced :: Int
  , assoc :: OperAssoc
  , exec :: ExecFunc
  }

derive instance operNewtype :: Newtype Oper _

-- | Operators are equal as long as everything except the `exec` is equal.
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
  , arity :: Int
  , exec :: ExecFunc
  }

derive instance funcNewtype :: Newtype Func _

-- | Functions are equal as long as everything except the `exec` is equal.
instance funcEq :: Eq Func where
  eq (Func { symbol: s, arity: a }) (Func { symbol: s', arity: a' }) =
    s == s'
      && a
      == a'

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
