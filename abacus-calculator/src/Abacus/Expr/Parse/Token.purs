module Abacus.Expr.Parse.Token
  ( exprCloseParen
  , exprComma
  , exprEq
  , exprFunc
  , exprLiteral
  , exprOpenParen
  , exprOper
  , exprSymb
  , exprVar
  ) where

import Prelude
import Abacus.Expr.Token (ExprToken(..), Func, Oper, Var(..))
import Abacus.Expr.Token.Default (equals)
import Abacus.Parse
  ( Parser
  , char
  , fail
  , floatS'
  , letter
  , lexeme
  , specialChar
  , word
  , (<?>)
  )
import Control.Monad.Reader (ReaderT(..))
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (fromCodePointArray)
import Data.String as S
import Global (readFloat)

exprEq :: Parser ExprToken
exprEq = ExprOper equals <$ lexeme (char '=') <?> "equals sign"

exprOper :: ReaderT (Array Oper) Parser ExprToken
exprOper =
  ReaderT
    $ \os -> do
        c <- lexeme specialChar <?> "operator"
        let
          x = find (unwrap >>> _.symbol >>> (_ == c)) os
        case x of
          Nothing -> fail $ S.singleton c <> " is not a valid operator"
          Just o -> pure $ ExprOper o

exprVar :: ReaderT (Array Var) Parser ExprToken
exprVar =
  ReaderT
    $ \vs -> do
        c <- lexeme letter <?> "variable"
        let
          x = find (unwrap >>> _.symbol >>> (_ == c)) vs
        case x of
          Nothing -> fail $ S.singleton c <> " is not a valid variable"
          Just (Var { val }) -> pure $ ExprLiteral val

exprFunc :: ReaderT (Array Func) Parser ExprToken
exprFunc =
  ReaderT
    $ \fs -> do
        s <- fromCodePointArray <$> lexeme word <?> "function"
        let
          x = find (unwrap >>> _.symbol >>> (_ == s)) fs
        case x of
          Nothing -> fail $ s <> " is not a valid function"
          Just f -> pure $ ExprFunc f

exprLiteral :: Parser ExprToken
exprLiteral =
  ExprLiteral
    <<< readFloat
    <<< fromCodePointArray
    <$> lexeme floatS'
    <?> "number"

exprSymb :: Parser ExprToken
exprSymb = ExprSymb <$> lexeme letter

exprOpenParen :: Parser ExprToken
exprOpenParen = ExprOpenParen <$ lexeme (char '(') <?> "\"(\""

exprCloseParen :: Parser ExprToken
exprCloseParen = ExprCloseParen <$ lexeme (char ')') <?> "\")\""

exprComma :: Parser ExprToken
exprComma = ExprComma <$ lexeme (char ',') <?> "comma"
