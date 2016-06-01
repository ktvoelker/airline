
module Command.ShowAllAircraft where

import Control.Lens
import qualified Data.Map as M
import H.Prelude

import Command
import Command.Util
import Object
import Types

data ShowAllAircraft = ShowAllAircraft
  deriving (Show)

type FlightResponse = (AirportCode, AirportCode, Distance, Distance)

type AircraftResponse = (AircraftCode, ModelCode, Either FlightResponse AirportCode)

data AircraftList = AircraftList [AircraftResponse]
  deriving (Show)

aircraftResponse :: Aircraft -> CSTM AircraftResponse
aircraftResponse aircraft = do
  AircraftState{..} <- readObject aircraft
  locationResponse <- either (fmap Left . flightResponse) (fmap Right . useObject apCode) _acLocation
  pure
    ( _acCode
    , view mCode _acModel
    , locationResponse
    )

flightResponse :: AircraftFlight -> CSTM FlightResponse
flightResponse flight = do
  AircraftFlightState{..} <- readObject flight
  let origin = view fOrigin _afsFlight
  let destination = view fDestination _afsFlight
  AirportState{_apCode = originCode} <- readObject origin
  AirportState{_apCode = destinationCode} <- readObject destination
  distance <- getFlightDistance origin destination
  pure (originCode, destinationCode, _afsTraveled, distance)

instance Command ShowAllAircraft where
  type Response ShowAllAircraft = AircraftList
  runCommand _ =
    fmap (AircraftList . M.elems) $ atomically $ getGame >>= useObject gAircraft >>= mapM aircraftResponse

