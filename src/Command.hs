
module Command where

import Control.Concurrent.STM
import Control.Lens
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import H.IO
import H.Prelude
import Prelude (subtract)
import System.Random

import Game
import Object
import Simulation
import Types
import Types.Command

randomAircraftCode :: (RandomGen g) => g -> S.Set AircraftCode -> AircraftCode
randomAircraftCode _ _ = AircraftCode $ "TODO"

airportResponse :: AirportState -> AirportResponse
airportResponse AirportState{..} = (_apCode, _apCapacity, S.size _apAircraft, _apPendingCount, _apName)

aircraftResponse :: AircraftState -> Maybe AirportState -> AircraftResponse
aircraftResponse AircraftState{..} airport = (_acCode, _acModel ^. mCode, fmap _apCode airport)

runCommand :: MasterHandle Game GamePart () () -> Game -> Command -> IO Response
runCommand mh game = \case
  Pause -> writeIORef (mhPaused mh) True >> pure NoResponse
  Resume -> writeIORef (mhPaused mh) False >> pure NoResponse
  Speed speed -> writeIORef (mhSpeed mh) (speedToCycleLength speed) >> pure NoResponse
  ShowAllAirports ->
    fmap (AirportList . map airportResponse . M.elems)
    $ atomically
    $ fmap (^. gAirports) (readObject game) >>= mapM readObject
  ShowAllAircraft ->
    fmap (AircraftList . map (uncurry aircraftResponse) . M.elems)
    $ atomically
    $ fmap (^. gAircraft) (readObject game)
      >>= mapM (readObject >=> \a -> (a,) <$> mapM readObject (a ^. acLocation))
  BuyAircraft modelCode airportCode -> do
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
  _ -> todo

