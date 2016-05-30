
module Command.BuyAircraft where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import H.Prelude

import Command
import Command.Monad
import Object
import Types

data BuyAircraft = BuyAircraft ModelCode AirportCode
  deriving (Show)

data BuyAircraftResponse =
    PurchasedAircraft AircraftCode
  | NotEnoughMoney
  | InvalidModel ModelCode
  | InvalidAirport AirportCode
  deriving (Show)

randomAircraftCode :: (RandomGen g) => g -> S.Set AircraftCode -> AircraftCode
randomAircraftCode _ _ = AircraftCode $ "TODO"

instance Command BuyAircraft where
  type Response BuyAircraft = BuyAircraftResponse
  runCommand (BuyAircraft modelCode airportCode) = do
    game <- getGame
    random <- newStdGen
    atomically $ do
      gameState <- readObject game
      let model = M.lookup modelCode $ gameState ^. gModels
      let airport = M.lookup airportCode $ gameState ^. gAirports
      case (model, airport) of
        (Nothing, _) -> pure $ InvalidModel modelCode
        (_, Nothing) -> pure $ InvalidAirport airportCode
        (Just model@Model{..}, Just airport) -> do
          if gameState ^. gMoney >= _mCost
          then do
            let aircraftCode = randomAircraftCode random $ M.keysSet $ gameState ^. gAircraft
            aircraft <- newObject
              $ AircraftState { _acCode = aircraftCode, _acModel = model, _acLocation = Just airport }
            modifyObject' airport $ over apAircraft (S.insert aircraft)
            modifyObject' game $ over gMoney (subtract _mCost) . over gAircraft (M.insert aircraftCode aircraft)
            pure $ PurchasedAircraft aircraftCode
          else pure NotEnoughMoney

