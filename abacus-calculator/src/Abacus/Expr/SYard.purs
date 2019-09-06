module Abacus.Expr.SYard where

import Prelude
import Abacus.Expr.Token (ExprToken(..), Oper(..), OperAssoc(..))
import Data.Array ((:))
import Data.Array as A
import Data.Foldable as F
import Data.Maybe (Maybe(..))

type SYardS
  = { output :: Array ExprToken
    , opers :: Array ExprToken
    }

infix2postfix :: Array ExprToken -> Maybe (Array ExprToken)
infix2postfix toks = fromS <$> F.foldM nextSYardS initS toks
  where
  initS = { output: [], opers: [] }

  fromS { output, opers } = A.reverse output <> opers

nextSYardS :: SYardS -> ExprToken -> Maybe SYardS
nextSYardS s@{ opers, output } tok = case tok of
  ExprLiteral _ -> Just $ s { output = tok : output }
  ExprFunc _ -> Just $ s { opers = tok : opers }
  ExprOpenParen -> Just $ s { opers = tok : opers }
  ExprOper oper -> pushOper oper s
  ExprCloseParen -> popOpersTill (_ == ExprOpenParen) s
  _ -> Just s

pushOper :: Oper -> SYardS -> Maybe SYardS
pushOper oper s@{ opers } = case A.uncons opers of
  Nothing -> Just s { opers = [ tok ] }
  Just { head, tail }
    | criteria oper head -> popOper s >>= pushOper oper
    | otherwise -> Just $ s { opers = tok : opers }
  where
  tok = ExprOper oper

-- | Criteria for popping an operator off the stack.
criteria :: Oper -> ExprToken -> Boolean
criteria _ (ExprFunc _) = true

criteria ( Oper oper
) (ExprOper (Oper { assoc: RightAssoc, preced })) =
  preced
    > oper.preced

criteria (Oper oper) (ExprOper (Oper { preced })) =
  preced
    >= oper.preced

criteria _ _ = false

popOpersTill :: (ExprToken -> Boolean) -> SYardS -> Maybe SYardS
popOpersTill pred s@{ opers } = do
  { head, tail } <- A.uncons opers
  if pred head then
    pure $ s { opers = tail }
  else
    popOper s >>= popOpersTill pred

popOper :: SYardS -> Maybe SYardS
popOper s@{ output, opers } = do
  { head, tail } <- A.uncons opers
  pure $ { output: head : output, opers: tail }
