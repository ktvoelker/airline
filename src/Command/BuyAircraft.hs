
module Command.BuyAircraft where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude

import Command
import Object
import Types

data BuyAircraft = BuyAircraft ModelCode AirportCode
  deriving (Show)

data BuyAircraftResponse = PurchasedAircraft AircraftCode
  deriving (Show)

randomAircraftCode :: (RandomGen g) => g -> S.Set AircraftCode -> AircraftCode
randomAircraftCode _ _ = AircraftCode $ "TODO"

instance Command BuyAircraft where
  type Response BuyAircraft = BuyAircraftResponse
  runCommand (BuyAircraft modelCode airportCode) = do
    random <- newStdGen
    atomically $ do
      game <- getGame
      gameState <- readObject game
      let model = M.lookup modelCode $ view gModels gameState
      let airport = M.lookup airportCode $ view gAirports gameState
      case (model, airport) of
        (Nothing, _) -> throwSTM $ InvalidModel modelCode
        (_, Nothing) -> throwSTM $ InvalidAirport airportCode
        (Just model@Model{..}, Just airport) -> do
          if view gMoney gameState >= _mCost
          then do
            let takenCodes = M.keysSet $ view gAircraft gameState
            let aircraftCode = randomAircraftCode random takenCodes
            aircraft <- newObject
              $ AircraftState
                { _acCode = aircraftCode
                , _acModel = model
                , _acLocation = Just airport
                }
            overObject' apAircraft (S.insert aircraft) airport
            overObject' gMoney (`minusMoney` _mCost) game
            overObject' gAircraft (M.insert aircraftCode aircraft) game
            pure $ PurchasedAircraft aircraftCode
          else throwSTM NotEnoughMoney

