module Main where

import Prelude
import Abacus.Expr.Defaults as Defaults
import Abacus.Expr.ExprToken (ExprToken, Func, Oper, createExprGroupParser')
import Abacus.Expr.Postfix (evalPostfix, infix2postfix)
import Abacus.Parse.Parser (runParser)
import Data.Either (Either(..), note)

type Options
  = { funcs :: Array Func
    , opers :: Array Oper
    , useDefFuncs :: Boolean
    , useDefOpers :: Boolean
    }

tokenize :: Options -> String -> Either String (Array ExprToken)
tokenize { funcs, opers, useDefFuncs, useDefOpers } s =
  let
    rslt = map (_.result) $ runParser (createExprGroupParser' opers' funcs') s
  in
    case rslt of
      Left err -> Left $ show err
      Right toks -> Right toks
  where
  funcs'
    | useDefFuncs = funcs <> Defaults.funcs
    | otherwise = funcs

  opers'
    | useDefOpers = opers <> Defaults.opers
    | otherwise = opers

calculate :: Options -> String -> Either String Number
calculate opts s = tokenize opts s >>= ((infix2postfix >=> evalPostfix) >>> note mempty)
