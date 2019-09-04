module Abacus.Parse.State
  ( State
  ) where

type State
  = { rem :: String
    , pos :: Int
    }
