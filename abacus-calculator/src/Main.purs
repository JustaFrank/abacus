module Main where

import Prelude
import Abacus.Expr.Defaults as Defaults
import Abacus.Expr.Eval (EvalState, evalPostfix)
import Abacus.Expr.Parse (expr)
import Abacus.Expr.SYard (infix2postfix)
import Abacus.Expr.Token (ExprToken, ExprEnv)
import Abacus.Parse.Parser (runParser)
import Control.Monad.State (runStateT)
import Data.Either (Either(..), note)
import Data.Tuple (Tuple)

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

calculate :: ExprEnv -> String -> Either String (Tuple Number EvalState)
calculate env s =
  tokenize env s
    >>= ( ( infix2postfix
            >=> ( \ts ->
                  runStateT (evalPostfix ts)
                    { env
                    , stack: []
                    }
              )
        )
          >>> note "ERROR!!"
      )
