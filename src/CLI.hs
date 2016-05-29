
module CLI where

import Control.Concurrent.STM
import Control.Lens
import Data.IORef
import qualified Data.Set as S
import qualified Data.Text as T
import System.Console.Haskeline
import H.IO
import H.Prelude
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
  <|> kw "show" *> kw "airports" *> pure ShowAllAirports

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
    airports <- atomically $ do
      airportObjects <- (^. gAirports) <$> readObject game
      mapM readObject airportObjects
    mapM_ (putStrLn . formatAirport) airports
    pure True
  _ -> todo

formatIntegral :: (Integral a, Show a) => a -> Int -> Text
formatIntegral n len = prefix <> digits
  where
    digits = show n
    remainingLen = len - T.length digits
    prefix = T.replicate remainingLen " "

formatAirport :: AirportState -> Text
formatAirport AirportState{..} =
  unAirportCode _apCode
  <> "   " <> formatIntegral _apCapacity 8
  <> "  " <> formatIntegral (S.size _apAircraft) 7
  <> "  " <> formatIntegral _apPendingCount 7
  <> "  " <> _apName

