
module Command.ShowAllAirports where

import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude

import Command
import Object
import Types

data ShowAllAirports = ShowAllAirports
  deriving (Show)

type AirportResponse = (AirportCode, Int, Int, Int, Text)

data AirportList = AirportList [AirportResponse]
  deriving (Show)

airportResponse :: AirportState -> AirportResponse
airportResponse AirportState{..} =
  ( _apCode
  , _apCapacity
  , S.size _apAircraft
  , _apPendingCount
  , _apName
  )

instance Command ShowAllAirports where
  type Response ShowAllAirports = AirportList
  type Error ShowAllAirports = ()
  runCommand _ = do
    game <- getGame
    fmap (AirportList . map airportResponse . M.elems)
      $ atomically
      $ liftSTM
      $ useObject gAirports game >>= mapM readObject

