module Abacus.Expr.Eval where

import Prelude
import Abacus.Expr.Token
  ( Computation
  , ExprEnv
  , ExprToken(..)
  , Func(..)
  , Oper(..)
  , TokenStack
  , execComp
  )
import Abacus.Utils.Array (splitAt)
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

type EvalResponse
  = { result :: Number
    , env :: ExprEnv
    }

eval :: ExprEnv -> TokenStack -> Maybe EvalResponse
eval env0 ts = do
  Tuple result { env: env1 } <- runStateT (evalS ts) initS
  Just { result, env: env1 }
  where
  initS = { env: env0, stack: [] }

evalS :: TokenStack -> StateT EvalState Maybe Number
evalS ts = traverse nextEvalState ts *> extractNumber

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

          args = A.reverse before
        Tuple n env1 <- runStateT (execComp comp args) env0
        Just $ pure $ pushToken (ExprLiteral n) { stack: after, env: env1 }

pushToken :: ExprToken -> EvalState -> EvalState
pushToken t s@{ stack } = s { stack = t : stack }
