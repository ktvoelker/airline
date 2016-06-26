
module Simulation where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.IORef
import Data.Time
import qualified Data.Text.IO as TIO
import H.Prelude
import H.IO

data Master =
  Master
  { mPaused    :: IORef Bool
  , mSpeed     :: IORef NominalDiffTime
  }

data MasterHandle =
  MasterHandle
  { mhThreadId :: ThreadId
  , mhPaused   :: IORef Bool
  , mhSpeed    :: IORef NominalDiffTime
  }

pausedSpeed :: NominalDiffTime
pausedSpeed = 0.1

forkSim :: NominalDiffTime -> IO () -> IO MasterHandle
forkSim startSpeed sim = do
  pausedRef <- newIORef True
  speedRef <- newIORef startSpeed
  masterThreadId <- forkIO $ runMaster (Master pausedRef speedRef) sim
  pure $ MasterHandle
    { mhThreadId = masterThreadId
    , mhPaused   = pausedRef
    , mhSpeed    = speedRef
    }

runMaster :: Master -> IO () -> IO ()
runMaster Master{..} sim = forever $ do
  -- Check if paused
  paused <- readIORef mPaused
  -- Read the current speed
  speed <- if paused then pure pausedSpeed else liftIO (readIORef mSpeed)
  -- Record the current time
  startTime <- getCurrentTime
  -- Run one simulation step if not paused
  when paused sim
  -- Calculate the remaining time for this cycle
  remaining <- (`subtract` speed) . (`diffUTCTime` startTime) <$> liftIO getCurrentTime
  -- If too much time has elapsed, log a warning
  -- Otherwise, sleep until enough time has elapsed
  if remaining < 0
    then liftIO $ TIO.putStrLn $ "Cycle took too long! " <> show remaining
    else liftIO $ threadDelay' remaining

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' = threadDelay . round . (* microsecondsPerSecond) . toRational

microsecondsPerSecond :: (Num a) => a 
microsecondsPerSecond = 1000000

