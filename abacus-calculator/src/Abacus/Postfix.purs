module Abacus.Postfix where

import Prelude

import Abacus.ExprToken (ExprToken(..), Func(..), Oper(..), OperAssoc(..))
import Data.Array (drop, reverse, take, uncons, (!!), (:))
import Data.Foldable (foldM)
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
  { output :: Array ExprToken
  , opers  :: Array ExprToken
  }

infix2postfix :: Array ExprToken -> Maybe (Array ExprToken)
infix2postfix toks = foldM nextS initS toks >>= state2output >>> Just
 where
  initS = { output: [], opers: [] }
  state2output { output, opers } = reverse output <> opers

nextS :: ShuntingS -> ExprToken -> Maybe ShuntingS
nextS s@{ opers, output } tok = case tok of
  ExprLiteral _  -> Just $ s { output = tok : output }
  ExprFunc    _  -> Just $ s { opers = tok : opers }
  ExprOpenParen  -> Just $ s { opers = tok : opers }
  ExprOper oper  -> pushOper s oper
  ExprCloseParen -> popUntilParen s
  _              -> Just s

pushOper :: ShuntingS -> Oper -> Maybe ShuntingS
pushOper s@{ opers: [] }     oper = Just $ s { opers = [ExprOper oper] }
pushOper s@{ output, opers } oper = do
  { head, tail } <- uncons opers
  if criteria oper head
    then pushOper (s { output = head : output, opers = tail }) oper
    else Just $ s { opers = (ExprOper oper) : opers }

popUntilParen :: ShuntingS -> Maybe ShuntingS
popUntilParen s@{ output, opers } = do
  { head, tail } <- uncons opers
  if head == ExprOpenParen
    then Just $ s { opers = tail }
    else popUntilParen (s { output = head : output, opers = tail })

criteria :: Oper -> ExprToken -> Boolean
criteria _ (ExprFunc _) = true
criteria (Oper oper) (ExprOper (Oper { assoc: RightAssoc, preced }))
  = preced > oper.preced
criteria (Oper oper) (ExprOper (Oper { preced })) = preced >= oper.preced
criteria _ _ = false
