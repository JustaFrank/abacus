module Abacus.Expr.Parse where

import Prelude
import Abacus.Expr.Parse.Token (exprCloseParen, exprComma, exprFunc, exprLiteral, exprOpenParen, exprOper)
import Abacus.Expr.Token (ExprToken(..), Func, Oper)
import Abacus.Parse (Parser, betweenI, char, eof, fail, floatS', lexeme, sepByI, specialChar, whitespace, word, (<?>))
import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Data.Array (many, (:))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (fromCodePointArray)
import Data.String as S
import Global (readFloat)

-- | Type that stores the environment for the expression parser.
type ExprEnv
  = { opers :: Array Oper
    , funcs :: Array Func
    }

-- | Parses an expression.
expr :: ExprEnv -> Parser (Array ExprToken)
expr env = many whitespace *> exprGroup env <* eof

-- | Parses an expression group, which consists of a term, followed by
-- | any number of operator-term pairs.
exprGroup :: ExprEnv -> Parser (Array ExprToken)
exprGroup env = join <$> sepByI (pure <$> exprOper env.opers) (term env)

term :: ExprEnv -> Parser (Array ExprToken)
term env = parenGroup env <|> funcGroup env <|> pure <$> exprLiteral

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
