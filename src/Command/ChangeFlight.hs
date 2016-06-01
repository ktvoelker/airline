
module Command.ChangeFlight where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude

import Command
import Object
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

lookupInGame :: (Ord k) => Getter GameState (M.Map k a) -> (k -> CommandError) -> k -> CSTM a
lookupInGame lens err key = getGame >>= useObject (lens . at key) >>= \case
  Nothing -> throw $ err key
  Just x -> pure x

lookupAirport :: AirportCode -> CSTM Airport
lookupAirport = lookupInGame gAirports InvalidAirport

lookupModel :: ModelCode -> CSTM Model
lookupModel = lookupInGame gModels InvalidModel

instance Command ChangeFlight where
  type Response ChangeFlight = ()
  runCommand ChangeFlight{..} = atomically $ do
    origin <- maybe (pure Nothing) (fmap Just . lookupAirport) cfOrigin
    destination <- maybe (pure Nothing) (fmap Just . lookupAirport) cfDestination
    model <- maybe (pure Nothing) (fmap Just . lookupModel) cfModel
    game <- getGame
    maybeFlight <- useObject (gFlights . at cfFlightNumber) game
    case maybeFlight of
      Nothing -> case (origin, destination, model, cfDaysOfWeek, cfTimeOfDay) of
        (Just origin, Just destination, Just model, Just daysOfWeek, Just timeOfDay) -> do
          let
          { newFlight =
              Flight
              { _fNumber      = cfFlightNumber
              , _fOrigin      = origin
              , _fDestination = destination
              , _fModel       = model
              , _fTimes       = map (\dow -> packTimeOfWeek dow timeOfDay) $ S.elems daysOfWeek
              }
          }
          validateFlight newFlight
          overObject gFlights (M.insert cfFlightNumber newFlight) game
        _ -> throw MissingParameter
      Just _ -> todo

validateFlight :: Flight -> CSTM ()
validateFlight = todo

