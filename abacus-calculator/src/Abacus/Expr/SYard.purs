module Abacus.Expr.SYard where

import Prelude
import Abacus.Expr.Token (ExprToken(..), Oper(..), OperAssoc(..), TokenStack)
import Control.Monad.State (StateT(..), get, modify_, runStateT)
import Data.Array ((:))
import Data.Array as A
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (fst)

type SYardState
  = { output :: TokenStack
    , opers :: TokenStack
    }

sYard :: TokenStack -> Maybe TokenStack
sYard ts = fst <$> runStateT (sYardS ts) initS
  where
  initS = { output: [], opers: [] }

sYardS :: TokenStack -> StateT SYardState Maybe TokenStack
sYardS ts = traverse nextSYardState ts *> extractOutput

extractOutput :: StateT SYardState Maybe TokenStack
extractOutput = get >>= \{ output, opers } -> pure $ A.reverse output <> opers

nextSYardState :: ExprToken -> StateT SYardState Maybe Unit
nextSYardState t = case t of
  ExprLiteral _ -> pushToOutput t
  ExprSymb _ -> pushToOutput t
  ExprFunc _ -> pushToOpers t
  ExprOpenParen -> pushToOpers t
  ExprOper o -> moveOpersWhile (criteria o) *> pushToOpers t
  ExprCloseParen -> moveOpersWhile (_ /= ExprOpenParen) *> popOper
  _ -> unit <$ get

moveOpersWhile :: (ExprToken -> Boolean) -> StateT SYardState Maybe Unit
moveOpersWhile pred = StateT $ (map >>> map) pure $ whileM isPred moveOper
  where
  isPred = isFirstOper pred

popOper :: StateT SYardState Maybe Unit
popOper = StateT $ \s -> A.tail s.opers <#> s { opers = _ } >>> pure

moveOper :: SYardState -> Maybe SYardState
moveOper s@{ output, opers } = do
  { head, tail } <- A.uncons opers
  pure $ { output: head : output, opers: tail }

pushToOutput :: ExprToken -> StateT SYardState Maybe Unit
pushToOutput t = modify_ $ \s@{ output } -> s { output = t : output }

pushToOpers :: ExprToken -> StateT SYardState Maybe Unit
pushToOpers t = modify_ $ \s@{ opers } -> s { opers = t : opers }

isFirstOper :: (ExprToken -> Boolean) -> SYardState -> Boolean
isFirstOper pred { opers } = fromMaybe false (pred <$> A.head opers)

criteria :: Oper -> ExprToken -> Boolean
criteria o t = case o, t of
  _, ExprFunc _ -> true
  Oper { preced: p1 }, ExprOper (Oper { assoc, preced: p2 })
    | RightAssoc <- assoc -> p2 > p1
    | otherwise -> p2 >= p1
  _, _ -> false

whileM :: forall a m. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
whileM pred f x
  | pred x = f x >>= whileM pred f
  | otherwise = pure x
