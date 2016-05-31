
module Command.Simple where

import Data.Time.Clock
import H.Prelude

import Command

data SetPaused = SetPaused Bool
  deriving (Show)

data SetSpeed = SetSpeed Speed
  deriving (Show)

data Speed = SpeedSlow | SpeedMedium | SpeedFast
  deriving (Eq, Ord, Enum, Bounded, Show)

speedToCycleLength :: Speed -> NominalDiffTime
speedToCycleLength = \case
  SpeedSlow   -> 0.20
  SpeedMedium -> 0.15
  SpeedFast   -> 0.10

instance Command SetPaused where
  type Response SetPaused = ()
  type Error SetPaused = ()
  runCommand (SetPaused paused) = setPaused paused

instance Command SetSpeed where
  type Response SetSpeed = ()
  type Error SetSpeed = ()
  runCommand (SetSpeed speed) = setSpeed $ speedToCycleLength speed

