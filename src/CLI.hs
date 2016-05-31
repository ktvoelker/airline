
module CLI where

import Control.Lens
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.Haskeline
import H.IO
import H.Prelude
import Text.Parsec.Applicative

import CLI.Output
import Command
import Command.BuyAircraft
import Command.Monad (runCM)
import Command.RemoveFlight
import Command.SetFlight
import Command.ShowAllAircraft
import Command.ShowAllAirports
import Command.Simple
import Game
import Simulation
import Types
import Types.Time

data GameCommand = forall a. (Command a, CLIResponse (Response a)) => GameCommand a

data CLICommand = CLIQuit | CLIGameCommand GameCommand

cliSettings :: Settings IO
cliSettings =
  defaultSettings
  { historyFile    = Just ".airline-history"
  , autoAddHistory = True
  }

runCLI :: MasterHandle Game GamePart -> Game -> IO ()
runCLI mh game = runInputT cliSettings $ f
  where
    f = do
      mXs <- fmap pack <$> getInputLine "> "
      whenJust mXs $ \xs -> do
        continue <- liftIO $ case parseCommand xs of
          Left err -> putStrLn err >> pure True
          Right cmd -> runCLICommand mh game cmd
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
  , "from"
  , "medium"
  , "on"
  , "pause"
  , "quit"
  , "remove"
  , "resume"
  , "sell"
  , "show"
  , "slow"
  , "speed"
  , "to"
  , "with"
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

str :: P Text
str = (^. wspValue) . snd <$> token TString

readable :: (Integral a, Read a) => P (Maybe a)
readable = read <$> str

oneCLICommand :: P CLICommand
oneCLICommand = cliCommand <* eof

cliCommand :: P CLICommand
cliCommand =
  kw "quit" *> pure CLIQuit
  <|> fmap CLIGameCommand command

command :: P GameCommand
command =
  kw "pause" *> pure (GameCommand $ SetPaused True)
  <|> kw "resume" *> pure (GameCommand $ SetPaused False)
  <|> kw "speed" *> (GameCommand . SetSpeed <$> speed)
  <|> kw "show" *> showCommand
  <|> kw "buy" *> buyCommand
  <|> kw "flight" *> flightCommand
  <|> kw "remove" *> removeCommand

showCommand :: P GameCommand
showCommand =
  kw "airports" *> pure (GameCommand ShowAllAirports)
  <|> kw "aircraft" *> pure (GameCommand ShowAllAircraft)

buyCommand :: P GameCommand
buyCommand =
  fmap GameCommand $ BuyAircraft <$> (kw "aircraft" *> modelCode) <*> (kw "at" *> airportCode)

flightCommand :: P GameCommand
flightCommand =
  f
  <$> flightNumber
  <*> (kw "from" *> airportCode)
  <*> (kw "to" *> airportCode)
  <*> (kw "with" *> modelCode)
  <*> (kw "on" *> daysOfWeek)
  <*> (kw "at" *> timeOfDay')
  where
    f Nothing _ _ _ _ _ = GameCommand $ Error "Invalid flight number."
    f _ _ _ _ Nothing _ = GameCommand $ Error "Invalid days."
    f _ _ _ _ _ Nothing = GameCommand $ Error "Invalid times."
    f (Just n) f t m (Just dows) (Just tod) = GameCommand $ SetFlight n f t m dows tod

removeCommand :: P GameCommand
removeCommand =
  f <$> (kw "flight" *> flightNumber)
  where
    f Nothing = GameCommand $ Error "Invalid flight number."
    f (Just n) = GameCommand $ RemoveFlight n

flightNumber :: P (Maybe FlightNumber)
flightNumber = fmap FlightNumber <$> readable

daysOfWeek :: P (Maybe (S.Set DayOfWeek))
daysOfWeek = fmap (S.fromList . concat) . sequence . map f . unpack . T.toLower <$> str
  where
    f = \case
      '*' -> Just [minBound .. maxBound]
      's' -> Just [Sunday]
      'm' -> Just [Monday]
      't' -> Just [Tuesday]
      'w' -> Just [Wednesday]
      'r' -> Just [Thursday]
      'f' -> Just [Friday]
      'a' -> Just [Saturday]
      _   -> Nothing

timeOfDay' :: P (Maybe TimeOfDay)
timeOfDay' = f <$> str
  where
    f xs = case T.splitOn ":" xs of
      [hoursText, minutesText] -> do
        hours <- g hoursText
        minutes <- g minutesText
        pure . timeOfDay $ (hours * 60) + minutes
      _ -> Nothing
    g = fmap fromInteger . read

modelCode :: P ModelCode
modelCode = ModelCode <$> str

airportCode :: P AirportCode
airportCode = AirportCode <$> str

speed :: P Speed
speed =
  kw "slow" *> pure SpeedSlow
  <|> kw "medium" *> pure SpeedMedium
  <|> kw "fast" *> pure SpeedFast

parseCommand :: Text -> Either Text CLICommand
parseCommand xs = either (Left . show) Right $ parse oneCLICommand (tokenize xs)

runCLICommand :: MasterHandle Game GamePart -> Game -> CLICommand -> IO Bool
runCLICommand mh game = \case
  CLIQuit -> pure False
  CLIGameCommand (GameCommand command) -> runCM (runCommand command) mh game >>= putStr . formatResponse >> pure True

