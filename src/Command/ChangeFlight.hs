
module Command.ChangeFlight where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude

import Command
import CrossMap as CM
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
  Nothing -> throwSTM $ err key
  Just x -> pure x

lookupAirport :: AirportCode -> CSTM Airport
lookupAirport = lookupInGame gAirports InvalidAirport

lookupModel :: ModelCode -> CSTM Model
lookupModel = lookupInGame gModels InvalidModel

getAirportDistance :: Airport -> Airport -> CSTM Distance
getAirportDistance a b = fmap (maybe (Distance 0) id) $ getGame >>= useObject (gDistances . to (CM.lookup a b))

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
              , _fDays        = daysOfWeek
              , _fTime        = timeOfDay
              }
          }
          validateFlight newFlight
          overObject gFlights (M.insert cfFlightNumber newFlight) game
        _ -> throw MissingParameter
      Just oldFlight -> do
        let
        { setOrigin = maybe id (set fOrigin) origin
        ; setDestination = maybe id (set fDestination) destination
        ; setModel = maybe id (set fModel) model
        ; setDays = maybe id (set fDays) cfDaysOfWeek
        ; setTime = maybe id (set fTime) cfTimeOfDay
        ; newFlight = setOrigin . setDestination . setModel . setDays . setTime $ oldFlight
        }
        validateFlight newFlight
        overObject gFlights (M.insert cfFlightNumber newFlight) game

minFlightDistance :: Distance
minFlightDistance = Distance { miles = 50 }

validateFlight :: Flight -> CSTM ()
validateFlight Flight{..} = do
  distance <- getAirportDistance _fOrigin _fDestination
  when (distance < minFlightDistance) $ throwSTM RouteTooShort

