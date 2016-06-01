
module Command.RemoveFlight where

import qualified Data.Map as M
import H.Prelude

import Command
import Object
import Types

data RemoveFlight = RemoveFlight FlightNumber
  deriving (Show)

instance Command RemoveFlight where
  type Response RemoveFlight = ()
  runCommand (RemoveFlight flightNumber) = atomically $ getGame >>= overObject gFlights (M.delete flightNumber)

