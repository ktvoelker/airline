
module Command.SetFlight where

import qualified Data.Set as S
import H.Prelude

import Command
import Types
import Types.Time

data SetFlight =
  SetFlight
  { sfFlightNumber :: FlightNumber
  , sfOrigin       :: AirportCode
  , sfDestination  :: AirportCode
  , sfModel        :: ModelCode
  , sfDaysOfWeek   :: S.Set DayOfWeek
  , sfTimeOfDay    :: TimeOfDay
  } deriving (Show)

data SetFlightResponse =
    SFFlightExists
  | SFInvalidOrigin
  | SFInvalidDestination
  | SFInvalidModel
  | SFTooShort
  deriving (Show)

data ChangeFlight =
  ChangeFlight
  { cfFlightNumber :: FlightNumber
  , cfOrigin       :: Maybe AirportCode
  , cfDestination  :: Maybe AirportCode
  , cfModel        :: Maybe ModelCode
  , cfDaysOfWeek   :: Maybe (S.Set DayOfWeek)
  , cfTimeOfDay    :: Maybe TimeOfDay
  } deriving (Show)

data ChangeFlightResponse =
    CFInvalidFlight
  | CFInvalidOrigin
  | CFInvalidDestination
  | CFInvalidModel
  | CFTooShort
  deriving (Show)

instance Command SetFlight where
  type Response SetFlight = SetFlightResponse
  runCommand _ = todo

instance Command ChangeFlight where
  type Response ChangeFlight = ChangeFlightResponse
  runCommand _ = todo

