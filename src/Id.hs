
module Id
  ( Id()
  , IdMap
  , IdSet
  , first
  , next
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

newtype Id a = Id Integer deriving (Eq, Ord, Show)

type IdMap a = M.Map (Id a) a

type IdSet a = S.Set (Id a)

firstId :: Id a
firstId = Id 0

nextId :: Id a -> Id a
nextId (Id n) = Id (n + 1)

