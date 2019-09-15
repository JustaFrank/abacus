module Abacus
  ( abacus
  , calculate
  , tokenize
  ) where

import Prelude
import Abacus.Expr.Eval (EvalResponse, eval)
import Abacus.Expr.Parse (runParseExpr)
import Abacus.Expr.SYard (sYard)
import Abacus.Expr.Token (TokenStack, ExprEnv)
import Abacus.Expr.Token.Default as Default
import Abacus.Parse (runParser)
import Data.Array as A
import Data.Either (Either(..), note)

type AbacusResponse
  = { result :: String
    , env :: ExprEnv
    }

-- | Same as `calculate`, but injects default functions/operators into env.
abacus :: ExprEnv -> String -> AbacusResponse
abacus env0 s =
  let
    env1 =
      env0
        { funcs = env0.funcs `A.union` Default.funcs
        , opers = env0.opers `A.union` Default.opers
        }
  in
    case tokenize env1 s
        >>= (sYard >=> eval env1)
        >>> note "Error evaluating expression." of
      Left err -> { result: err, env: env1 }
      Right { result, env: env2 } -> { result: show result, env: env2 }

calculate :: ExprEnv -> String -> Either String EvalResponse
calculate env s =
  tokenize env s
    >>= (sYard >=> eval env)
    >>> note "Error evaluating expression."

tokenize :: ExprEnv -> String -> Either String TokenStack
tokenize env s = case runParser (runParseExpr $ addDefaults env) s of
  Left err -> Left $ show err
  Right { result } -> Right result

addDefaults :: ExprEnv -> ExprEnv
addDefaults env =
  env
    { funcs = env.funcs <> Default.funcs
    , opers = env.opers <> Default.opers
    }
