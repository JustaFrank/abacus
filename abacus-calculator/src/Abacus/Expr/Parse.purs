module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Defaults as Defaults
import Abacus.Expr.Parse.Token (exprCloseParen, exprComma, exprEq, exprFunc, exprLiteral, exprOpenParen, exprOper, exprSymb, exprVar)
import Abacus.Expr.Token (ExprEnv, ExprToken(..))
import Abacus.Parse (Parser, betweenI, eof, not, sepByI, sepByITill, whitespace)
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Data.Array ((:))
import Data.List (many)

-- | Parses an expression.
expr :: ExprEnv -> Parser (Array ExprToken)
expr env =
  many whitespace
    *> ( varDecl
          <|> ( join
                <$> sepByITill
                    (pure <$> exprOper env.opers)
                    eof
                    (implMult env <|> term env)
            )
      )
  where
  varDecl = do
    s <- exprSymb
    eq <- exprEq
    x <- expr env
    pure $ [ s ] <> [ eq ] <> x

-- | Parses an expression group, which consists of a term, followed by
-- | any number of operator-term pairs.
exprGroup :: ExprEnv -> Parser (Array ExprToken)
exprGroup env = join <$> sepByI (pure <$> exprOper env.opers) (term env <|> implMult env)

term :: ExprEnv -> Parser (Array ExprToken)
term env =
  (pure <$> exprLiteral)
    <|> (pure <$> exprVar env.vars)
    <|> parenGroup env
    <|> funcGroup env

implMult :: ExprEnv -> Parser (Array ExprToken)
implMult env = not (exprLiteral *> exprLiteral) implMult'
  where
  implMult' = do
    t1 <- term env
    t2 <- term env
    pure $ t1 <> [ ExprOper Defaults.omult ] <> t2

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
