
module Command.Util where

import Control.Lens
import qualified Data.Map as M
import H.Prelude

import Command
import qualified CrossMap as CM
import Object
import Types

lookupInGame :: (Ord k) => Getter GameState (M.Map k a) -> (k -> CommandError) -> k -> CSTM a
lookupInGame lens err key = getGame >>= useObject (lens . at key) >>= \case
  Nothing -> throwSTM $ err key
  Just x -> pure x

lookupAirport :: AirportCode -> CSTM Airport
lookupAirport = lookupInGame gAirports InvalidAirport

lookupModel :: ModelCode -> CSTM Model
lookupModel = lookupInGame gModels InvalidModel

getFlightDistance :: Airport -> Airport -> CSTM Distance
getFlightDistance a b = fmap (maybe (Distance 0) id) $ getGame >>= useObject (gDistances . to (CM.lookup a b))

