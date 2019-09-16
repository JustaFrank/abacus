module Abacus.Utils.Monad
  ( whileM
  ) where

import Prelude

whileM :: forall a m. Monad m => (a -> Boolean) -> (a -> m a) -> a -> m a
whileM pred f x
  | pred x = f x >>= whileM pred f
  | otherwise = pure x
