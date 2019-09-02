module Abacus.Postfix where

import Prelude

import Abacus.ExprToken (ExprToken(..), Func(..), Oper(..), OperAssoc(..), ExecFunc)
import Data.Array (drop, reverse, take, uncons, (!!), (:))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

---------------------------------------------------------------------------
-- Postfix Evaluator

evalPostfix :: Array ExprToken -> Maybe Number
evalPostfix toks = foldM nextPEvalS [] toks >>= (_ !! 0) >>= getLiteral

nextPEvalS :: Array ExprToken -> ExprToken -> Maybe (Array ExprToken)
nextPEvalS stack tok = case tok of
  ExprLiteral _ -> Just $ tok : stack
  ExprOper (Oper { exec }) -> execFunc exec 2 stack
  ExprFunc (Func { arity, exec }) -> execFunc exec arity stack
  _ -> Nothing

execFunc :: ExecFunc -> Int -> Array ExprToken -> Maybe (Array ExprToken)
execFunc f arity stack =
  let { before: top, after: rem } = splitAt arity stack
  in  (_ : rem) <<< ExprLiteral <$> (f =<< traverse getLiteral (reverse top))

getLiteral :: ExprToken -> Maybe Number
getLiteral tok = case tok of
  ExprLiteral v -> Just v
  _ -> Nothing

splitAt :: forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: take n xs, after: drop n xs }

---------------------------------------------------------------------------
-- Infix to Postfix

type ShuntingS =
  { output :: Array ExprToken
  , opers  :: Array ExprToken
  }

infix2postfix :: Array ExprToken -> Maybe (Array ExprToken)
infix2postfix toks = foldM nextSYardS initS toks >>= state2output >>> Just
 where
  initS = { output: [], opers: [] }
  state2output { output, opers } = reverse output <> opers

nextSYardS :: ShuntingS -> ExprToken -> Maybe ShuntingS
nextSYardS s@{ opers, output } tok = case tok of
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
