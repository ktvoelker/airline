
module Game where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.STM.Class
import qualified Data.Map as M
import qualified Data.Set as S
import H.IO
import H.Prelude

import Object
import Parallel
import Types
import Types.Time

gameSim :: Game -> IO ()
gameSim g = do
  parallelize splitByAirport (prepare g) nullJoiner g
  landingAirportsVar <- newTVarIO []
  parallelize splitByAirport (executeTakeoff g landingAirportsVar) nullJoiner g
  parallelize splitByAircraftFlight (flyAircraft g) nullJoiner g
  landingAirports <- atomically $ readTVar landingAirportsVar
  parallelize (const $ pure landingAirports) (executeLanding g) nullJoiner g

splitByAirport :: (Functor m, MonadSTM m) => Game -> m [Airport]
splitByAirport g = view (gAirports . to M.elems) <$> readObject g

splitByAircraftFlight :: (Functor m, MonadSTM m) => Game -> m [AircraftFlight]
splitByAircraftFlight g = view (gAirborne . to S.toList) <$> readObject g

nullJoiner :: Game -> [()] -> IO ()
nullJoiner = const . const $ pure ()

prepare :: Game -> Airport -> IO ()
prepare g ap = do
  enqueueTakeoffs g ap
  overObject apMovementDelay (max (Minutes 0) . plusMinutes (Minutes (-1))) ap

enqueueTakeoffs :: Game -> Airport -> IO ()
enqueueTakeoffs g ap = do
  timeOfWeek <- snd . unpackAbsoluteTime <$> useObject gTime g
  allTakeoffs <- maybe [] S.toList <$> useObject (gSchedule . at timeOfWeek) g
  let takeoffs = filter ((== ap) . view fOrigin) allTakeoffs
  mapM_ (enqueueTakeoff ap) takeoffs

enqueueTakeoff :: Airport -> Flight -> IO ()
enqueueTakeoff ap f = do
  let model = view fModel f
  allAircraft <- S.toList <$> useObject apAircraft ap
  matchingAircraft <- filterM (aircraftModelIs model) allAircraft
  case matchingAircraft of
    [] -> pure ()
    (chosenAircraft : _) -> atomically $ do
      overObject apAircraft (S.delete chosenAircraft) ap
      af <- newObject
        $ AircraftFlightState
        { _afsAircraft = chosenAircraft
        , _afsFlight = f
        , _afsTraveled = Distance 0
        }
      pending <- useObject apPending ap
      writeTQueue pending $ Takeoff af

aircraftModelIs :: Model -> Aircraft -> IO Bool
aircraftModelIs desiredModel ac = do
  actualModel <- useObject acModel ac
  pure $ actualModel == desiredModel

executeTakeoff :: Game -> TVar [Airport] -> Airport -> IO ()
executeTakeoff g landingAirports ap = atomically $ do
  delay <- useObject apMovementDelay ap
  when (delay == Minutes 0) $ do
    movements <- useObject apPending ap
    tryReadTQueue movements >>= \case
      Just (Takeoff af) -> overObject gAirborne (S.insert af) g
      _ -> modifyTVar landingAirports (ap :)

flyAircraft :: Game -> AircraftFlight -> IO ()
flyAircraft = todo

executeLanding :: Game -> Airport -> IO ()
executeLanding _ ap = atomically $ do
  movements <- useObject apPending ap
  tryReadTQueue movements >>= \case
    Nothing -> pure ()
    Just (Landing af) -> do
      aircraft <- useObject afsAircraft af
      overObject apAircraft (S.insert aircraft) ap
    Just (Takeoff _) -> error "Impossible: unexpected takeoff in queue!"
