module Main where

import Prelude
import Abacus (abacus)
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
      let
        { result, env: env1 } = abacus env0 s
      log result
      setLineHandler interface $ printResult interface env1
      prompt interface

  initEnv = { opers: [], funcs: [], vars: [] }
