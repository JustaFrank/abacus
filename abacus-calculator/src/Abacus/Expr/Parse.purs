module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Parse.Token
  ( exprCloseParen
  , exprComma
  , exprFunc
  , exprLiteral
  , exprOpenParen
  , exprOper
  , exprVar
  )
import Abacus.Expr.Token (ExprToken, Func, Oper, Var)
import Abacus.Parse (Parser, betweenI, eof, sepByI, whitespace)
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Data.Array (many, (:))

-- | Type that stores the environment for the expression parser.
type ExprEnv
  = { opers :: Array Oper
    , funcs :: Array Func
    , vars :: Array Var
    }

-- | Parses an expression.
expr :: ExprEnv -> Parser (Array ExprToken)
expr env = many whitespace *> exprGroup env <* eof

-- | Parses an expression group, which consists of a term, followed by
-- | any number of operator-term pairs.
exprGroup :: ExprEnv -> Parser (Array ExprToken)
exprGroup env = join <$> sepByI (pure <$> exprOper env.opers) (term env)

term :: ExprEnv -> Parser (Array ExprToken)
term env =
  (pure <$> exprLiteral)
    <|> (pure <$> exprVar env.vars)
    <|> parenGroup env
    <|> funcGroup env

parenGroup :: ExprEnv -> Parser (Array ExprToken)
parenGroup env = parenI $ defer (\_ -> exprGroup env)

funcGroup :: ExprEnv -> Parser (Array ExprToken)
funcGroup env = (:) <$> exprFunc env.funcs <*> commaGroup env

commaGroup :: ExprEnv -> Parser (Array ExprToken)
commaGroup env = parenI $ join <$> sepByI sep (defer $ \_ -> term env)
  where
  sep = pure <$> (exprOper env.opers <|> exprComma)

-- | Parses between parentheses, including parentheses.
parenI :: Parser (Array ExprToken) -> Parser (Array ExprToken)
parenI = betweenI (pure <$> exprOpenParen) (pure <$> exprCloseParen)
