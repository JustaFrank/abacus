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
import Data.Either (Either(..), note)

abacus :: ExprEnv -> String -> Either String EvalResponse
abacus env s =
  tokenize env s
    >>= (sYard >=> eval env)
    >>> note "Error evaluating expression."

calculate :: ExprEnv -> String -> Either String Number
calculate env s =
  tokenize env s
    >>= (sYard >=> eval env)
    >>> map (_.result)
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
