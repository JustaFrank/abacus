module Abacus.Expr.Eval where

import Prelude
import Abacus.Expr.Token (Computation, ExprEnv, ExprToken(..), Func(..), Oper(..), TokenStack, execComp)
import Control.Monad.State (StateT(..), get, lift, modify_, runStateT)
import Control.Plus (empty)
import Data.Array ((:))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type EvalState
  = { stack :: TokenStack
    , env :: ExprEnv
    }

eval :: TokenStack -> StateT EvalState Maybe Number
eval ts = traverse nextEvalState ts *> extractNumber

-- eval ts = foldr (\t s -> (s *> nextEvalState t)) initS ts *> extractNumber
--   where
--   initS = pure unit
extractNumber :: StateT EvalState Maybe Number
extractNumber = get >>= _.stack >>> (A.head >=> getNumber) >>> lift

getNumber :: ExprToken -> Maybe Number
getNumber t = case t of
  ExprLiteral n -> Just n
  _ -> Nothing

nextEvalState :: ExprToken -> StateT EvalState Maybe Unit
nextEvalState t = case t of
  ExprLiteral _ -> modify_ $ pushToken t
  ExprSymb _ -> modify_ $ pushToken t
  ExprOper (Oper { comp }) -> execCompFromStack comp
  ExprFunc (Func { comp }) -> execCompFromStack comp
  _ -> empty

execCompFromStack :: Computation -> StateT EvalState Maybe Unit
execCompFromStack comp@{ arity } =
  StateT
    $ \{ stack, env: env0 } -> do
        let
          { before, after } = splitAt arity stack
        Tuple n env1 <- runStateT (execComp comp $ A.reverse before) env0
        Just $ pure $ pushToken (ExprLiteral n) { stack, env: env1 }

pushToken :: ExprToken -> EvalState -> EvalState
pushToken t s@{ stack } = s { stack = t : stack }

splitAt ::
  forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: A.take n xs, after: A.drop n xs }
