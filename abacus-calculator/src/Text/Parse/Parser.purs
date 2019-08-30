module Text.Parse.Parser
  ( Parser(..)
  , anyOf
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Text.Parse.State (State)

---------------------------------------------------------------------------
-- Parser

-- TODO: Currently using an Array for errors. Consider List for performance.
-- TODO: Create Error type that allows for monoid operations.

newtype Parser a = Parser (String -> Either (Array String) (State a))

instance parserFunctor :: Functor Parser where
  map = pmap

instance parserApply :: Apply Parser where
  apply = papply

instance parserApplicative :: Applicative Parser where
  pure = pid

instance parserBind :: Bind Parser where
  bind = pbind

instance parserMonad :: Monad Parser

instance parserAlt :: Alt Parser where
  alt = por

instance parserPlus :: Plus Parser where
  empty = pfail

instance parserAlternative :: Alternative Parser

---------------------------------------------------------------------------
-- Combinators

anyOf :: forall a. Array (Parser a) -> Parser a
anyOf = foldl alt empty

---------------------------------------------------------------------------
-- Combinator Aliases (use typeclass methods)

papply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
papply (Parser p) (Parser q) = Parser $ \s -> do
  { rest: prest, token: f } <- p s
  { rest: qrest, token: qtok } <- q prest
  Right { rest: qrest, token: f qtok }

pbind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
pbind (Parser p) f = Parser $ \s -> do
  { rest: prst, token: ptok } <- p s
  let Parser q = f ptok
  q prst

pfail :: forall a. Parser a
pfail = Parser $ \_ -> Left mempty

pid :: forall a. a -> Parser a
pid x = Parser $ \s -> Right { rest: s, token: x }

por :: forall a. Parser a -> Parser a -> Parser a
por (Parser p) (Parser q) = Parser $ \s ->
  let pres = p s
  in  case pres of
    Right pstate -> Right pstate
    Left  perr   -> case q s of
      Right qstate -> Right qstate
      Left  qerr   -> Left $ perr <> qerr

pmap :: forall a b. (a -> b) -> Parser a -> Parser b
pmap f (Parser p) = Parser $ \s -> do
  state <- p s
  Right state { token = f state.token }
