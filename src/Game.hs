
module Game where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import H.IO
import H.Prelude

import Object
import Parallel
import Types

gameSim :: Game -> IO ()
gameSim g = do
  parallelize splitByAirport (enqueueTakeoffs g) nullJoiner g
  (takeoffAirports, landingAirports) <- partitionByNextMovement g
  parallelize (const $ pure takeoffAirports) (executeTakeoff g) nullJoiner g
  parallelize splitByAircraftFlight (flyAircraft g) nullJoiner g
  parallelize (const $ pure landingAirports) (executeLanding g) nullJoiner g

splitByAirport :: Game -> IO [Airport]
splitByAirport g = view (gAirports . to M.elems) <$> readObject g

splitByAircraftFlight :: Game -> IO [AircraftFlight]
splitByAircraftFlight g = view (gAirborne . to S.toList) <$> readObject g

nullJoiner :: Game -> [()] -> IO ()
nullJoiner = const . const $ pure ()

enqueueTakeoffs :: Game -> Airport -> IO ()
enqueueTakeoffs = todo

partitionByNextMovement :: Game -> IO ([Airport], [Airport])
partitionByNextMovement = todo

executeTakeoff :: Game -> Airport -> IO ()
executeTakeoff = todo

flyAircraft :: Game -> AircraftFlight -> IO ()
flyAircraft = todo

executeLanding :: Game -> Airport -> IO ()
executeLanding = todo
