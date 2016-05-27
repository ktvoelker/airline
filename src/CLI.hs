
module CLI where

import System.Console.Haskeline
import H.IO
import H.Prelude

data Command =
    Pass
  | Quit
  deriving (Eq, Show)

runCLI :: IO ()
runCLI = runInputT defaultSettings $ f
  where
    f = do
      mXs <- fmap pack <$> getInputLine "> "
      whenJust mXs $ \xs -> do
        continue <- liftIO $ case parseCommand xs of
          Left err -> putStrLn err >> pure True
          Right cmd -> runCommand cmd
        when continue $ f

parseCommand :: Text -> Either Text Command
parseCommand = \case
  "pass" -> Right Pass
  "quit" -> Right Quit
  _ -> Left "Parse error."

runCommand :: Command -> IO Bool
runCommand = \case
  Pass -> pure True
  Quit -> pure False

