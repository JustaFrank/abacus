module Abacus.Parse.State
  ( State
  ) where

type State a
  = { rest :: String
    , token :: a
    }
