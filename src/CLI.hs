
module CLI where

import Control.Concurrent.STM
import Data.IORef
import System.Console.Haskeline
import H.IO
import H.Prelude

import Game
import Simulation
import Types
import Types.Command

runCLI :: MasterHandle Game GamePart CoreCommand Response -> IO ()
runCLI mh = runInputT defaultSettings $ f
  where
    f = do
      mXs <- fmap pack <$> getInputLine "> "
      whenJust mXs $ \xs -> do
        continue <- liftIO $ case parseCommand xs of
          Left err -> putStrLn err >> pure True
          Right cmd -> runCommand mh cmd
        when continue $ f

parseCommand :: Text -> Either Text Command
parseCommand = \case
  "pass" -> Right Pass
  "quit" -> Right Quit
  "pause" -> Right Pause
  "resume" -> Right Resume
  "speed slow" -> Right (Speed SpeedSlow)
  "speed medium" -> Right (Speed SpeedMedium)
  "speed fast" -> Right (Speed SpeedFast)
  "core pass" -> Right (Core CorePass)
  _ -> Left "Parse error."

runCommand :: MasterHandle Game GamePart CoreCommand Response -> Command -> IO Bool
runCommand mh = \case
  Pass -> pure True
  Quit -> pure False
  Pause -> writeIORef (mhPaused mh) True >> pure True
  Resume -> writeIORef (mhPaused mh) False >> pure True
  Speed speed -> writeIORef (mhSpeed mh) (speedToCycleLength speed) >> pure True
  Core cmd -> do
    atomically (writeTChan (mhCommands mh) cmd)
    atomically (readTChan (mhResponses mh)) >>= putStrLn . show
    pure True

