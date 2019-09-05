module Abacus.Parse.Parser
  ( Parser(..)
  , ParseResponse
  , ParseState
  , fail
  , labelParser
  , runParser
  , (<?>)
  ) where

import Prelude
import Abacus.Parse.Error (ParseError(..))
import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Lazy (class Lazy)
import Data.Either (Either(..))

-- | Run a parser on String input.
runParser :: forall a. Parser a -> String -> ParseResponse a
runParser (Parser p) = p <<< { rem: _, pos: 0 }

-- | Label a parser with what it expects to parse.
labelParser :: forall a. Parser a -> String -> Parser a
labelParser (Parser p) label =
  Parser
    $ \s -> case p s of
        Right res -> Right res
        Left e -> case e of
          BasicError be -> Left $ BasicError be { expt = [ label ] }
          CustomError { pos } ->
            Left
              $ BasicError
                  { pos
                  , unexpt: mempty
                  , expt: [ label ]
                  }

infixr 3 labelParser as <?>

-- | Fails with a custom error.
fail :: forall a. String -> Parser a
fail msg = Parser $ \{ pos } -> Left $ CustomError { pos, msgs: [ msg ] }

-- | Return type of a parser.
type ParseResponse a
  = Either ParseError { result :: a, state :: ParseState }

-- | State that is passed from one parser to the next.
type ParseState
  = { rem :: String
    , pos :: Int
    }

-- | `Parser datatype. `a` is the result type of the parser.
newtype Parser a
  = Parser (ParseState -> ParseResponse a)

-- | The `Lazy` typeclass is required for `many` and `some`.
derive newtype instance parserLazy :: Lazy (Parser a)

instance parserFunctor :: Functor Parser where
  map = pMap

pMap :: forall a b. (a -> b) -> Parser a -> Parser b
pMap f (Parser p) =
  Parser
    $ \s -> do
        state <- p s
        Right state { result = f state.result }

instance parserApply :: Apply Parser where
  apply = pApply

pApply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
pApply (Parser p) (Parser q) =
  Parser
    $ \s -> do
        { state: pstate, result: f } <- p s
        { state: qstate, result: qres } <- q pstate
        Right { state: qstate, result: f qres }

instance parserApplicative :: Applicative Parser where
  pure = pId

pId :: forall a. a -> Parser a
pId x = Parser $ \state -> Right { state, result: x }

instance parserBind :: Bind Parser where
  bind = pBind

pBind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
pBind (Parser p) f =
  Parser
    $ \s -> do
        { state: pstate, result: prslt } <- p s
        let
          Parser q = f prslt
        q pstate

instance parserMonad :: Monad Parser

instance parserAlt :: Alt Parser where
  alt = pAlt

pAlt :: forall a. Parser a -> Parser a -> Parser a
pAlt (Parser p) (Parser q) =
  Parser
    $ \s -> case p s of
        Right pstate -> Right pstate
        Left perr -> case q s of
          Right qstate -> Right qstate
          Left qerr -> Left $ perr <> qerr

instance parserPlus :: Plus Parser where
  empty = pEmpty

pEmpty :: forall a. Parser a
pEmpty = Parser $ \_ -> Left mempty

instance parserAlternative :: Alternative Parser
