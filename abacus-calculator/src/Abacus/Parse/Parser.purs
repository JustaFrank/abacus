module Abacus.Parse.Parser
  ( Parser(..)
  , ParseResponse
  , anyOf
  , labelParser
  , runParser
  ) where

import Prelude
import Abacus.Parse.Error (ParseError(..))
import Abacus.Parse.State (State)
import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Lazy (class Lazy)
import Data.Either (Either(..))
import Data.Foldable (foldl)

---------------------------------------------------------------------------
-- Parser
-- TODO: Currently using an Array for errors. Consider List for performance.
-- TODO: Create Error type that allows for monoid operations.
runParser :: forall a. Parser a -> String -> ParseResponse a
runParser (Parser p) = p <<< { rem: _, pos: 0 }

labelParser :: forall a. Parser a -> String -> Parser a
labelParser (Parser p) label =
  Parser
    $ \s -> case p s of
        Left (ParseError err) ->
          Left
            $ ParseError
            $ err { expected = [ label ] }
        r -> r

type ParseResponse a
  = Either ParseError { result :: a, state :: State }

newtype Parser a
  = Parser (State -> ParseResponse a)

derive newtype instance parserLazy :: Lazy (Parser a)

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
papply (Parser p) (Parser q) =
  Parser
    $ \s -> do
        { state: pstate, result: f } <- p s
        { state: qstate, result: qres } <- q pstate
        Right { state: qstate, result: f qres }

pbind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
pbind (Parser p) f =
  Parser
    $ \s -> do
        { state: pstate, result: prslt } <- p s
        let
          Parser q = f prslt
        q pstate

pfail :: forall a. Parser a
pfail = Parser $ \_ -> Left mempty

pid :: forall a. a -> Parser a
pid x = Parser $ \state -> Right { state, result: x }

por :: forall a. Parser a -> Parser a -> Parser a
por (Parser p) (Parser q) =
  Parser
    $ \s -> case p s of
        Right pstate -> Right pstate
        Left perr -> case q s of
          Right qstate -> Right qstate
          Left qerr -> Left $ perr <> qerr

pmap :: forall a b. (a -> b) -> Parser a -> Parser b
pmap f (Parser p) =
  Parser
    $ \s -> do
        state <- p s
        Right state { result = f state.result }
