
module Command.Simple where

import Data.Time.Clock
import H.Prelude

import Command
import Command.Monad

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
  runCommand (SetPaused paused) = lift $ setPaused paused

instance Command SetSpeed where
  type Response SetSpeed = ()
  type Error SetSpeed = ()
  runCommand (SetSpeed speed) = lift $ setSpeed $ speedToCycleLength speed

