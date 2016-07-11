
module Game where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.STM.Class
import qualified Data.List as L
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
  parallelize splitByAirport (enqueueTakeoffs g) nullJoiner g
  (takeoffAirports, landingAirports) <- atomically $ partitionByNextMovement g
  parallelize (const $ pure takeoffAirports) (executeTakeoff g) nullJoiner g
  parallelize splitByAircraftFlight (flyAircraft g) nullJoiner g
  parallelize (const $ pure landingAirports) (executeLanding g) nullJoiner g

splitByAirport :: (Functor m, MonadSTM m) => Game -> m [Airport]
splitByAirport g = view (gAirports . to M.elems) <$> readObject g

splitByAircraftFlight :: (Functor m, MonadSTM m) => Game -> m [AircraftFlight]
splitByAircraftFlight g = view (gAirborne . to S.toList) <$> readObject g

nullJoiner :: Game -> [()] -> IO ()
nullJoiner = const . const $ pure ()

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

partitionByNextMovement :: Game -> STM ([Airport], [Airport])
partitionByNextMovement g = do
  airports <- splitByAirport g
  airportsKeyed <- mapM (\ap -> (ap,) <$> nextMovementIsTakeoff ap) airports
  let (ts, fs) = L.partition snd airportsKeyed
  pure (map fst ts, map fst fs)

nextMovementIsTakeoff :: Airport -> STM Bool
nextMovementIsTakeoff ap = do
  movements <- useObject apPending ap
  tryPeekTQueue movements >>= \case
    Nothing -> pure False
    Just (Takeoff _) -> pure True
    Just (Landing _) -> pure False

executeTakeoff :: Game -> Airport -> IO ()
executeTakeoff = todo

flyAircraft :: Game -> AircraftFlight -> IO ()
flyAircraft = todo

executeLanding :: Game -> Airport -> IO ()
executeLanding = todo
