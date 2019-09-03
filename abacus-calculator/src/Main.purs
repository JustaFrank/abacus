module Main where

import Prelude

import Abacus.ExprToken (ExprToken, Func, Oper, createExprGroupParser)
import Abacus.Parse.Defaults as Defaults
import Abacus.Parse.Parser (runParser)
import Abacus.Postfix (evalPostfix, infix2postfix)
import Data.Either (Either, note)

type Options =
  { funcs :: Array Func
  , opers :: Array Oper
  , useDefFuncs :: Boolean
  , useDefOpers :: Boolean
  }

tokenize :: Options -> String -> Either (Array String) (Array ExprToken)
tokenize { funcs, opers, useDefFuncs, useDefOpers }
  = runParser (createExprGroupParser opers' funcs') >>> map (_.token)
 where
  funcs'
    | useDefFuncs = funcs <> Defaults.funcs
    | otherwise   = funcs
  opers'
    | useDefOpers = opers <> Defaults.opers
    | otherwise   = opers

calculate :: Options -> String -> Either (Array String) Number
calculate opts s =
  tokenize opts s >>= ((infix2postfix >=> evalPostfix) >>> note [])
