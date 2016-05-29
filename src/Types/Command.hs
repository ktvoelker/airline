
module Types.Command where

import Data.Time.Clock
import H.Prelude

import Types

data Speed = SpeedSlow | SpeedMedium | SpeedFast
  deriving (Eq, Ord, Enum, Bounded, Show)

speedToCycleLength :: Speed -> NominalDiffTime
speedToCycleLength = \case
  SpeedSlow   -> 0.20
  SpeedMedium -> 0.15
  SpeedFast   -> 0.10

data Command =
    Quit
  | Pause
  | Resume
  | Speed Speed
  | BuyAircraft ModelCode AirportCode
  | ShowAllAirports
  | ShowAirport AirportCode
  | ShowAllAircraft
  | ShowAircraft AircraftCode
  | ShowAllCities
  | ShowAllModels
  | ShowModel ModelCode
  deriving (Eq, Ord, Show)

