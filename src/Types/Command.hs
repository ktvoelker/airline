
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
    Error Text
  | Pause
  | Resume
  | Speed Speed
  | BuyAircraft ModelCode AirportCode
  | SetFlight FlightNumber AirportCode AirportCode ModelCode [TimeOfWeek]
  | RemoveFlight FlightNumber
  | ShowAllAirports
  | ShowAirport AirportCode
  | ShowAllAircraft
  | ShowAircraft AircraftCode
  | ShowAllCities
  | ShowAllModels
  | ShowModel ModelCode
  deriving (Eq, Ord, Show)

type AirportResponse = (AirportCode, Int, Int, Int, Text)

type AircraftResponse = (AircraftCode, ModelCode, Maybe AirportCode)

data Response =
    NoResponse
  | ErrorResponse Text
  | AirportList [AirportResponse]
  | AircraftList [AircraftResponse]
  | PurchasedAircraft AircraftCode
  | NotEnoughMoney
  | InvalidModel ModelCode
  | InvalidAirport AirportCode
  deriving (Eq, Ord)

