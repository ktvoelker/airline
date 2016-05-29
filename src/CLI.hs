
module CLI where

import Control.Concurrent.STM
import Control.Lens
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.Haskeline
import H.IO
import H.Prelude
import Prelude (subtract)
import System.Random
import Text.Parsec.Applicative

import Game
import Object
import Simulation
import Types
import Types.Command

runCLI :: MasterHandle Game GamePart () () -> Game -> IO ()
runCLI mh game = runInputT defaultSettings $ f
  where
    f = do
      mXs <- fmap pack <$> getInputLine "> "
      whenJust mXs $ \xs -> do
        continue <- liftIO $ case parseCommand xs of
          Left err -> putStrLn err >> pure True
          Right cmd -> runCommand mh game cmd
        when continue $ f

data TT = TKeyword Text | TString
  deriving (Eq, Ord, Show)

keywords :: Set Text
keywords =
  S.fromList
  [ "aircraft"
  , "airport"
  , "airports"
  , "at"
  , "buy"
  , "create"
  , "fast"
  , "flight"
  , "flights"
  , "medium"
  , "pause"
  , "quit"
  , "remove"
  , "resume"
  , "sell"
  , "show"
  , "slow"
  , "speed"
  ]

type TD = WithSourcePos Text

tokenize :: Text -> [(TT, TD)]
tokenize = f (initialPos Nothing)
  where
    f _ "" = []
    f pos xs = (tokenType, WithSourcePos tokenText tokenPos) : f nextPos xs''
      where
        (spacePrefix, xs') = T.span isSpace xs
        (tokenText, xs'') = T.span (not . isSpace) xs'
        tokenPos = updatePosString pos (unpack spacePrefix)
        nextPos = updatePosString tokenPos (unpack tokenText)
        tokenType = if tokenText `S.member` keywords then TKeyword tokenText else TString

type P a = Parser () TT TD a

kw :: Text -> P ()
kw xs = void $ token (TKeyword xs)

oneCommand :: P Command
oneCommand = command <* eof

command :: P Command
command =
  kw "quit" *> pure Quit
  <|> kw "pause" *> pure Pause
  <|> kw "resume" *> pure Resume
  <|> kw "speed" *> (Speed <$> speed)
  <|> kw "show" *> showCommand
  <|> kw "buy" *> buyCommand

showCommand :: P Command
showCommand =
  kw "airports" *> pure ShowAllAirports
  <|> kw "aircraft" *> pure ShowAllAircraft

buyCommand :: P Command
buyCommand =
  (BuyAircraft <$> (kw "aircraft" *> modelCode) <*> (kw "at" *> airportCode))

modelCode :: P ModelCode
modelCode = ModelCode . (^. wspValue) . snd <$> token TString

airportCode :: P AirportCode
airportCode = AirportCode . (^. wspValue) . snd <$> token TString

speed :: P Speed
speed =
  kw "slow" *> pure SpeedSlow
  <|> kw "medium" *> pure SpeedMedium
  <|> kw "fast" *> pure SpeedFast

parseCommand :: Text -> Either Text Command
parseCommand xs = either (Left . show) Right $ parse oneCommand (tokenize xs)

runCommand :: MasterHandle Game GamePart () () -> Game -> Command -> IO Bool
runCommand mh game = \case
  Quit -> pure False
  Pause -> writeIORef (mhPaused mh) True >> pure True
  Resume -> writeIORef (mhPaused mh) False >> pure True
  Speed speed -> writeIORef (mhSpeed mh) (speedToCycleLength speed) >> pure True
  ShowAllAirports -> do
    putStrLn "Code  Capacity  Present  Pending  Name"
    airports <- atomically $ fmap (^. gAirports) (readObject game) >>= mapM readObject
    mapM_ (putStrLn . formatAirport) airports
    pure True
  ShowAllAircraft -> do
    putStrLn "Code   Model  Location"
    aircraft <- atomically
      $ fmap (^. gAircraft) (readObject game)
      >>= mapM (readObject >=> \a -> (a,) <$> mapM readObject (a ^. acLocation))
    mapM_ (putStrLn . uncurry formatAircraft) aircraft
    pure True
  BuyAircraft modelCode airportCode -> do
    random <- newStdGen
    outcome <- atomically $ do
      gameState <- readObject game
      let model = M.lookup modelCode $ gameState ^. gModels
      let airport = M.lookup airportCode $ gameState ^. gAirports
      case (model, airport) of
        (Nothing, _) -> pure "Invalid model."
        (_, Nothing) -> pure "Invalid airport."
        (Just model@Model{..}, Just airport) -> do
          if gameState ^. gMoney >= _mCost
          then do
            let aircraftCode = randomAircraftCode random $ M.keysSet $ gameState ^. gAircraft
            aircraft <- newObject
              $ AircraftState { _acCode = aircraftCode, _acModel = model, _acLocation = Just airport }
            modifyObject' airport $ over apAircraft (S.insert aircraft)
            modifyObject' game $ over gMoney (subtract _mCost) . over gAircraft (M.insert aircraftCode aircraft)
            pure "Purchased."
          else pure "Not enough money!"
    putStrLn outcome
    pure True
  _ -> todo

randomAircraftCode :: (RandomGen g) => g -> S.Set AircraftCode -> AircraftCode
randomAircraftCode _ _ = AircraftCode $ "TODO"

formatField :: Bool -> Int -> Text -> Text
formatField rightJustified fieldLength xs = (if rightJustified then (affix <>) else (<> affix)) xs
  where
    remainingLen = fieldLength - T.length xs
    affix = T.replicate remainingLen " "

formatIntegral :: (Integral a, Show a) => Int -> a -> Text
formatIntegral fieldLength = formatField True fieldLength . show

formatAirport :: AirportState -> Text
formatAirport AirportState{..} =
  formatField False 3 (unAirportCode _apCode)
  <> "   " <> formatIntegral 8 _apCapacity
  <> "  " <> formatIntegral 7 (S.size _apAircraft)
  <> "  " <> formatIntegral 7 _apPendingCount
  <> "  " <> _apName

formatAircraft :: AircraftState -> Maybe AirportState -> Text
formatAircraft AircraftState{..} ap =
  formatField False 5 (unAircraftCode _acCode) 
  <> "  " <> formatField False 5 (unModelCode $ _acModel ^. mCode)
  <> "  " <> formatField False 3 (maybe "---" (unAirportCode . (^. apCode)) ap)

