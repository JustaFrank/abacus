module Abacus.Expr.Token where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (CodePoint)
import Data.String as S

-- | Expression token datatype. A token can be a literal (number), operator,
-- | function, parentheses, or comma.
data ExprToken
  = ExprLiteral Number
  | ExprOper Oper
  | ExprFunc Func
  | ExprOpenParen
  | ExprCloseParen
  | ExprComma

derive instance eqExprToken :: Eq ExprToken

instance showExprToken :: Show ExprToken where
  show (ExprLiteral n) = show n
  show (ExprOper oper) = show oper
  show (ExprFunc func) = show func
  show ExprOpenParen = "("
  show ExprCloseParen = ")"
  show ExprComma = ","

-- | Type signature for functions. The array input accounts for multiple
-- | parameters.
type ExecFunc
  = Array Number -> Maybe Number

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
