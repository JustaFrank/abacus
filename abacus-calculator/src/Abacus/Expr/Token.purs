module Abacus.Expr.Token where

import Prelude
import Control.Monad.State (StateT, lift)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (CodePoint)
import Data.String as S

type ExprEnv
  = { opers :: Array Oper
    , funcs :: Array Func
    , vars :: Array Var
    }

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

newtype Oper
  = Oper
  { symbol :: CodePoint
  , preced :: Int
  , assoc :: OperAssoc
  , comp :: Computation
  }

derive instance operNewtype :: Newtype Oper _

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

data OperAssoc
  = LeftAssoc
  | RightAssoc

derive instance operAssocEq :: Eq OperAssoc

derive instance operAssocGeneric :: Generic OperAssoc _

instance operAssocShow :: Show OperAssoc where
  show = genericShow

newtype Func
  = Func
  { symbol :: String
  , comp :: Computation
  }

derive instance funcNewtype :: Newtype Func _

instance funcEq :: Eq Func where
  eq (Func { symbol: s }) (Func { symbol: s' }) = s == s'

instance funcShow :: Show Func where
  show (Func { symbol }) = symbol

newtype Var
  = Var
  { symbol :: CodePoint
  , val :: Number
  }

derive instance varNewtype :: Newtype Var _

derive instance varEq :: Eq Var

instance varShow :: Show Var where
  show (Var { symbol }) = S.singleton symbol

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
