
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
import Game
import Simulation
import Types
import Types.Command

data CLICommand = Quit | GameCommand Command
  deriving (Eq, Ord, Show)

runCLI :: MasterHandle Game GamePart () () -> Game -> IO ()
runCLI mh game = runInputT defaultSettings $ f
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

oneCLICommand :: P CLICommand
oneCLICommand = cliCommand <* eof

cliCommand :: P CLICommand
cliCommand =
  kw "quit" *> pure Quit
  <|> fmap GameCommand command

command :: P Command
command =
  kw "pause" *> pure Pause
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

parseCommand :: Text -> Either Text CLICommand
parseCommand xs = either (Left . show) Right $ parse oneCLICommand (tokenize xs)

runCLICommand :: MasterHandle Game GamePart () () -> Game -> CLICommand -> IO Bool
runCLICommand mh game = \case
  Quit -> pure False
  GameCommand command -> runCommand mh game command >>= putStr . formatResponse >> pure True

