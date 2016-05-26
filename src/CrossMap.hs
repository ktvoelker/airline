
module CrossMap
  ( CrossMap()
  , empty
  , insert
  , lookup
  ) where

import qualified Data.Map as M
import H.Prelude hiding (empty)

newtype CrossMap a b = CrossMap (M.Map (a, a) b) deriving (Eq, Ord, Show)

empty :: CrossMap a b
empty = CrossMap M.empty

insert :: (Ord a) => a -> a -> b -> CrossMap a b -> CrossMap a b
insert k1 k2 v (CrossMap m) = CrossMap $ M.insert (sortPair k1 k2) v m

lookup :: (Ord a) => a -> a -> CrossMap a b -> Maybe b
lookup k1 k2 (CrossMap m) = M.lookup (sortPair k1 k2) m

sortPair :: (Ord a) => a -> a -> (a, a)
sortPair a b = case a > b of
  True  -> (b, a)
  False -> (a, b)

