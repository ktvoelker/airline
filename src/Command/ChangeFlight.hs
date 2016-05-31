
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

data ChangeFlightError =
    CFMissingParameter
  | CFInvalidOrigin
  | CFInvalidDestination
  | CFInvalidModel
  | CFTooShort
  deriving (Show)

lookupInGame :: (Ord k) => Getter GameState (M.Map k a) -> e -> k -> CSTM e a
lookupInGame lens err key = get >>= liftSTM . useObject (lens . at key) >>= \case
  Nothing -> throwError err
  Just x -> pure x

lookupAirport :: e -> AirportCode -> CSTM e Airport
lookupAirport = lookupInGame gAirports

lookupModel :: e -> ModelCode -> CSTM e Model
lookupModel = lookupInGame gModels

instance Command ChangeFlight where
  type Response ChangeFlight = ()
  type Error ChangeFlight = ChangeFlightError
  runCommand ChangeFlight{..} = atomically' $ do
    origin <- maybe (pure Nothing) (fmap Just . lookupAirport CFInvalidOrigin) cfOrigin
    destination <- maybe (pure Nothing) (fmap Just . lookupAirport CFInvalidDestination) cfDestination
    model <- maybe (pure Nothing) (fmap Just . lookupModel CFInvalidModel) cfModel
    game <- get
    maybeFlight <- liftSTM $ useObject (gFlights . at cfFlightNumber) game
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
          liftSTM $ overObject gFlights (M.insert cfFlightNumber newFlight) game
        _ -> throwError CFMissingParameter
      Just _ -> todo

validateFlight :: Flight -> CSTM ChangeFlightError ()
validateFlight = todo

