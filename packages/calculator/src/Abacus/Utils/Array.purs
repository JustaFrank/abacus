module Abacus.Utils.Array (splitAt) where

import Data.Array as A

splitAt ::
  forall a. Int -> Array a -> { before :: Array a, after :: Array a }
splitAt n xs = { before: A.take n xs, after: A.drop n xs }
