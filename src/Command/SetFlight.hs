
module Command.SetFlight where

import H.Prelude

import Command
import Types

data SetFlight = SetFlight FlightNumber AirportCode AirportCode ModelCode [TimeOfWeek]
  deriving (Show)

instance Command SetFlight where
  type Response SetFlight = ()
  runCommand _ = todo

