
module Types.Command where

import Data.Time.Clock
import H.Prelude

data Speed = SpeedSlow | SpeedMedium | SpeedFast
  deriving (Eq, Ord, Enum, Bounded, Show)

speedToCycleLength :: Speed -> NominalDiffTime
speedToCycleLength = \case
  SpeedSlow   -> 0.20
  SpeedMedium -> 0.15
  SpeedFast   -> 0.10

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

