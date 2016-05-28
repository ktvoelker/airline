
module Types.Command where

import Data.Time.Clock
import H.Prelude

data Speed = SpeedSlow | SpeedMedium | SpeedFast
  deriving (Eq, Ord, Enum, Bounded, Show)

speedToCycleLength :: Speed -> NominalDiffTime
speedToCycleLength = \case
  SpeedSlow   -> 1.0
  SpeedMedium -> 0.6
  SpeedFast   -> 0.2

data Command =
    Pass
  | Quit
  | Pause
  | Resume
  | Speed Speed
  | Core CoreCommand
  deriving (Eq, Ord, Show)

data CoreCommand =
    CorePass
  deriving (Eq, Ord, Show)

data Response =
    Empty
  deriving (Eq, Ord, Show)

