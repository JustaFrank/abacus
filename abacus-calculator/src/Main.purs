module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"

test :: (Number -> Number) -> Number -> Number
test f x = f x

test2 :: Number -> Number
test2 x = x + 1.0