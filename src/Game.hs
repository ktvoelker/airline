
module Game where

import H.IO
import H.Prelude

import Simulation
import Types
import Types.Command

newtype GamePart = GamePart Game
  deriving (Eq, Ord)

handleCommand :: CoreCommand -> StateT Game IO Response
handleCommand = const $ pure Empty

gameSim :: Sim Game GamePart CoreCommand Response
gameSim =
  Sim
  { simPart  = pure
  , simSplit = \g -> [GamePart g]
  , simMerge = \[GamePart g] _ -> g
  , simFinal = pure
  , simUser  = handleCommand
  }

