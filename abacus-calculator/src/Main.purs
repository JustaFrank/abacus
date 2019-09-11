module Main where

import Prelude
import Abacus (abacus)
import Abacus.Expr.Token.Default as Default
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "abacus> " 2 interface
  prompt interface
  setLineHandler interface $ printResult interface initEnv
  where
  printResult interface env0 s =
    if s == ":q" || s == ":Q" then
      close interface
    else do
      case abacus env0 s of
        Left err -> log err
        Right { result, env: env1 } -> do
          log $ show result
          setLineHandler interface $ printResult interface env1
      prompt interface

  initEnv = { opers: Default.opers, funcs: Default.funcs, vars: [] }
