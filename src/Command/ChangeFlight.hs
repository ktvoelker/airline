
module Command.ChangeFlight where

import qualified Data.Set as S
import H.Prelude

import Command
import Types
import Types.Time

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
    CFMissingParameter
  | CFInvalidOrigin
  | CFInvalidDestination
  | CFInvalidModel
  | CFTooShort
  deriving (Show)

instance Command ChangeFlight where
  type Response ChangeFlight = ChangeFlightResponse
  runCommand _ = todo

