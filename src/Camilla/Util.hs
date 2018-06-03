module Camilla.Util where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (toList, fromList, HashMap)

revLookup :: (Eq v) => v -> [(k, v)] -> Maybe k
revLookup k = lookup k . map (\(x, y) -> (y, x))

traverseKVs
    :: (Hashable k, Eq k, Applicative f)
    => (a -> f k) -> (b -> f v) -> HashMap a b -> f (HashMap k v)
traverseKVs kf vf =
    fmap fromList . traverse (\(k, v) -> (,) <$> kf k <*> vf v) . toList

