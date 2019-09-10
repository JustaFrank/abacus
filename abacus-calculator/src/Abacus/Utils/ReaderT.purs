module Abacus.Utils.ReaderT
  ( applyReaderT
  , deferReaderT
  ) where

import Prelude
import Control.Lazy (class Lazy, defer)
import Control.Monad.Reader (ReaderT(..), mapReaderT, runReaderT)

applyReaderT ::
  forall r m a b.
  Monad m =>
  ReaderT r (Function (m a)) (m b) -> ReaderT r m a -> ReaderT r m b
applyReaderT rdr1 rdr2 =
  ReaderT
    $ \r -> runReaderT (runReaderT rdr1 r `mapReaderT` rdr2) r

deferReaderT ::
  forall r m a.
  Monad m =>
  Lazy (m a) => (Unit -> ReaderT r m a) -> ReaderT r m a
deferReaderT rdr = ReaderT $ \r -> defer (\_ -> runReaderT (rdr unit) r)
