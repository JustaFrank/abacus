module Abacus.Postfix where

import Prelude

import Abacus.ExprToken (ExprToken(..), Func(..), Oper(..), OperAssoc(..))
import Data.Array (drop, reverse, tail, take, uncons, (!!), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

---------------------------------------------------------------------------
-- Evaluating Postfix

evalPostfix :: Array ExprToken -> Maybe Number
evalPostfix = evalPostfix' []

evalPostfix' :: Array ExprToken -> Array ExprToken -> Maybe Number
evalPostfix' stack []
  = case stack !! 0 of
      Just (ExprLiteral n) -> Just n
      _  -> Nothing
evalPostfix' stack toks = do
  { head, tail } <- uncons toks
  case head of
    ExprLiteral _ -> evalPostfix' (head : stack) tail
    ExprOper (Oper oper) -> do
      y <- oper.exec <$> (stack !! 1 >>= getLiteral) <*> (stack !! 0 >>= getLiteral)
      evalPostfix'
        (ExprLiteral y : drop 2 stack)
        tail
    ExprFunc (Func func) -> do
      y <- func.exec =<< traverse getLiteral (reverse $ take func.arity stack)
      evalPostfix'
        (ExprLiteral y : drop func.arity stack)
        tail
    _ -> Nothing

getLiteral :: ExprToken -> Maybe Number
getLiteral tok = case tok of
  ExprLiteral v -> Just v
  _ -> Nothing

---------------------------------------------------------------------------
-- Infix to Postfix

type ShuntingS =
  { input  :: Array ExprToken
  , output :: Array ExprToken
  , opers  :: Array ExprToken
  }

infix2postfix :: Array ExprToken -> Maybe (Array ExprToken)
infix2postfix toks
  = _.output <$> infix2postfix' { input: toks, output: [], opers: [] }

infix2postfix' :: ShuntingS -> Maybe ShuntingS
infix2postfix' s@{ input: [], output, opers }
  = Just $ s { output = reverse $ opers <> output }
infix2postfix' s@{ input, output, opers } = do
  { head: tok, tail: rest } <- uncons input
  s' <- nextS tok rest s
  infix2postfix' s'

nextS :: ExprToken -> Array ExprToken -> ShuntingS -> Maybe ShuntingS
nextS tok@(ExprLiteral _) rest s@{ output }
  = Just $ s { input = rest, output = tok : output }
nextS tok@(ExprFunc _) rest s@{ opers }
  = Just $ s { input = rest, opers = tok : opers }
nextS tok@ExprOpenParen rest s@{ opers }
  = Just $ s { input = rest, opers = tok : opers }
nextS tok@(ExprOper (Oper oper)) rest s = do
  s' <- whileM pred operToOutput s
  pure $ s' { input = rest, opers = tok : s'.opers }
 where
  pred { opers } = case uncons opers of
    Just { head: (ExprFunc _) } -> true
    Just { head: (ExprOper (Oper { assoc: RightAssoc, preced })) } ->
      preced > oper.preced
    Just { head: (ExprOper (Oper { assoc: LeftAssoc, preced })) } ->
      preced >= oper.preced
    _ -> false
nextS tok@(ExprCloseParen) rest s = do
  s'       <- whileM pred operToOutput s
  remOpers <- tail s'.opers
  pure $ s' { input = rest, opers = remOpers }
 where
  pred { opers } = case uncons opers of
    Nothing -> false
    Just { head: ExprOpenParen } -> false
    _ -> true
nextS _ rest s = Just $ s { input = rest }

operToOutput :: ShuntingS -> Maybe ShuntingS
operToOutput s@{ output, opers } = do
  { head: tok, tail: rest } <- uncons opers
  Just $ s { output = tok : output, opers = rest }

whileM :: forall m a. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
whileM pred f x
  | pred x    = f x >>= whileM pred f
  | otherwise = pure x