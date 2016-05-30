
module Game where

import H.Prelude

import Simulation
import Types

newtype GamePart = GamePart Game
  deriving (Eq, Ord)

gameSim :: Sim Game GamePart
gameSim =
  Sim
  { simPart  = pure
  , simSplit = \g -> [GamePart g]
  , simMerge = \[GamePart g] _ -> g
  , simFinal = pure
  }

