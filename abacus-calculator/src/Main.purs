module Main where

import Prelude
import Abacus.Expr.Defaults as Defaults
import Abacus.Expr.Eval (EvalResponse, eval)
import Abacus.Expr.Parse (expr)
import Abacus.Expr.SYard (sYard)
import Abacus.Expr.Token (ExprEnv, ExprToken)
import Abacus.Parse.Parser (runParser)
import Data.Either (Either(..), note)

tokenize :: ExprEnv -> String -> Either String (Array ExprToken)
tokenize env s =
  let
    env' = env { funcs = Defaults.funcs, opers = Defaults.opers }

    rslt =
      map (_.result)
        $ runParser
            ( expr env'
            )
            s
  in
    case rslt of
      Left err -> Left $ show err
      Right toks -> Right toks

calculate :: ExprEnv -> String -> Either String EvalResponse
-- calculate :: ExprEnv -> String -> Either String TokenStack
calculate env s =
  tokenize env s
    >>= ( ( sYard
            >=> eval env
        )
          >>> note "ERROR!!"
      )
