
module CLI where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.Haskeline
import H.IO
import H.Prelude
import Text.Parsec.Applicative

import CLI.Output
import Command
import Command.BuyAircraft
import Command.ChangeFlight
import Command.RemoveFlight
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
str = view wspValue . snd <$> token TString

strPred :: Text -> (Text -> Bool) -> P Text
strPred _ predFunc =
  view wspValue . snd <$> token' TString (predicate () predFunc')
  where
    predFunc' = predFunc . view wspValue

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
  fmap GameCommand
  $ ChangeFlight
  <$> flightNumber
  <*> option (kw "from" *> airportCode)
  <*> option (kw "to" *> airportCode)
  <*> option (kw "with" *> modelCode)
  <*> option (kw "on" *> daysOfWeek)
  <*> option (kw "at" *> timeOfDay')

removeCommand :: P GameCommand
removeCommand = GameCommand . RemoveFlight <$> (kw "flight" *> flightNumber)

flightNumber :: P FlightNumber
flightNumber =
  FlightNumber
  . fromJust
  . readInt
  <$> strPred "flight-number" (isJust . readInt)
  where
    readInt :: Text -> Maybe Int
    readInt = read

daysOfWeekMap :: M.Map Char [DayOfWeek]
daysOfWeekMap =
  M.fromList
  [ ('*', [minBound .. maxBound])
  , ('s', [Sunday])
  , ('m', [Monday])
  , ('t', [Tuesday])
  , ('w', [Wednesday])
  , ('r', [Thursday])
  , ('f', [Friday])
  , ('a', [Saturday])
  ]

daysOfWeek :: P (S.Set DayOfWeek)
daysOfWeek =
  S.fromList
  . concat
  . map (daysOfWeekMap M.!)
  . unpack
  . T.toLower
  <$> strPred "days-of-week" (\xs -> T.all (`M.member` daysOfWeekMap) xs)

parseTimeOfDay :: Text -> Maybe (Integer, Integer)
parseTimeOfDay xs = case T.splitOn ":" xs of
  [hoursText, minutesText] -> do
    hours <- g hoursText
    minutes <- g minutesText
    if hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60
    then pure (hours, minutes)
    else Nothing
  _ -> Nothing
  where
    g = fmap fromInteger . read

timeOfDay' :: P TimeOfDay
timeOfDay' = f <$> strPred "time-of-day" (isJust . parseTimeOfDay)
  where
    f xs = case parseTimeOfDay xs of
      Just (hours, minutes) -> timeOfDay . Minutes $ (hours * 60) + minutes
      Nothing -> undefined

modelCode :: P ModelCode
modelCode = ModelCode <$> str

airportCode :: P AirportCode
airportCode = AirportCode <$> str

speed :: P SimulationSpeed
speed =
  kw "slow" *> pure SpeedSlow
  <|> kw "medium" *> pure SpeedMedium
  <|> kw "fast" *> pure SpeedFast

parseCommand :: Text -> Either Text CLICommand
parseCommand xs = either (Left . show) Right $ parse oneCLICommand (tokenize xs)

handleCommandError :: CommandError -> IO ()
handleCommandError = putStrLn . show

runCLICommand :: MasterHandle Game GamePart -> Game -> CLICommand -> IO Bool
runCLICommand mh game = \case
  CLIQuit -> pure False
  CLIGameCommand (GameCommand command) -> do
    (runCM (runCommand command) mh game >>= putStr . formatResponse) `catch` handleCommandError
    pure True

