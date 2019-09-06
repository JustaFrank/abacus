module Abacus.Expr.Eval where

import Prelude
import Abacus.Expr.Token (ExecFunc, ExprEnv, ExprToken(..), Func(..), Oper(..))
import Control.Monad.State (StateT(..), modify_, runStateT)
import Control.Plus (empty)
import Data.Array (foldl, (:), (!!))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type EvalState
  = { stack :: TokenStack
    , env :: ExprEnv
    }

type TokenStack
  = Array ExprToken

type Arity
  = Int

evalPostfix :: TokenStack -> StateT EvalState Maybe Number
evalPostfix ts = getFirstLiteral $ foldl (\s t -> s >>= (\_ -> nextEvalState t)) initS ts
  where
  initS = StateT $ \s -> Just $ Tuple unit s

getFirstLiteral :: forall a. StateT EvalState Maybe a -> StateT EvalState Maybe Number
getFirstLiteral s =
  StateT
    $ \s0 -> case runStateT s s0 of
        Nothing -> Nothing
        Just (Tuple _ s1@{ stack }) -> case getLiteral =<< stack !! 0 of
          Nothing -> Nothing
          Just n -> Just $ Tuple n s1

getLiteral :: ExprToken -> Maybe Number
getLiteral tok = case tok of
  ExprLiteral v -> Just v
  _ -> Nothing

nextEvalState :: ExprToken -> StateT EvalState Maybe Unit
nextEvalState tok = case tok of
  ExprLiteral _ -> modify_ $ \s@{ stack } -> s { stack = tok : stack }
  ExprSymb _ -> modify_ $ \s@{ stack } -> s { stack = tok : stack }
  ExprOper (Oper { exec }) -> execComputeFunc exec 2
  ExprFunc (Func { arity, exec }) -> execComputeFunc exec arity
  _ -> empty

execComputeFunc :: ExecFunc -> Arity -> StateT EvalState Maybe Unit
execComputeFunc f arity =
  StateT
    $ \s@{ env: env0, stack } ->
        let
          { before: args, after: rem } = splitAt arity stack
        in
          case runStateT (f $ A.reverse args) env0 of
            Nothing -> Nothing
            Just (Tuple n env1) ->
              Just
                $ pure { env: env1, stack: ExprLiteral n : rem }

splitAt ::
  forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: A.take n xs, after: A.drop n xs }
