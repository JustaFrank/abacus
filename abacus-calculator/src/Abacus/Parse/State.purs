module Abacus.Parse.State
  ( State
  ) where

type State a
  = { result :: a
    , input :: String
    }
