
module Command.ShowAllAircraft where

import Control.Lens
import qualified Data.Map as M
import H.Prelude

import Command
import Object
import Types

data ShowAllAircraft = ShowAllAircraft
  deriving (Show)

type AircraftResponse = (AircraftCode, ModelCode, Maybe AirportCode)

data AircraftList = AircraftList [AircraftResponse]
  deriving (Show)

aircraftResponse :: AircraftState -> Maybe AirportState -> AircraftResponse
aircraftResponse AircraftState{..} airport =
  ( _acCode
  , view mCode _acModel
  , fmap _apCode airport
  )

instance Command ShowAllAircraft where
  type Response ShowAllAircraft = AircraftList
  runCommand _ = do
    game <- getGame
    fmap (AircraftList . map (uncurry aircraftResponse) . M.elems)
      $ atomically
      $ useObject gAircraft game
        >>= mapM (readObject >=> \a -> (a,) <$> mapM readObject (view acLocation a))

