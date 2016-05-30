
module Simulation where

import Control.Concurrent (ThreadId, forkIO, getNumCapabilities, threadDelay)
import qualified Control.Concurrent.MSemN as Sem
import Control.Concurrent.STM
import Data.IORef
import Data.List (replicate)
import Data.Time
import qualified Data.Text.IO as TIO
import H.Prelude
import H.IO

data Sim g p =
  Sim
  { simPart  :: p -> IO p
  , simSplit :: g -> [p]
  , simMerge :: [p] -> g -> g
  , simFinal :: g -> IO g
  }

data Master g p =
  Master
  { mParts     :: TChan p
  , mPaused    :: IORef Bool
  , mSpeed     :: IORef NominalDiffTime
  , mCounter   :: Sem.MSemN Int
  , mWorkers   :: [ThreadId]
  }

data MasterHandle g p =
  MasterHandle
  { mhThreadId :: ThreadId
  , mhPaused   :: IORef Bool
  , mhSpeed    :: IORef NominalDiffTime
  }

pausedSpeed :: NominalDiffTime
pausedSpeed = 0.1

forkSim :: NominalDiffTime -> Sim g p -> g -> IO (MasterHandle g p)
forkSim startSpeed sim st = do
  pausedRef <- newIORef True
  speedRef <- newIORef startSpeed
  masterThreadId <- forkIO $ do
    n <- getNumCapabilities
    partsChan <- newTChanIO
    sem <- Sem.new 0
    let worker = runWorker (simPart sim) partsChan (Sem.signal sem 1)
    Master partsChan pausedRef speedRef sem
      <$> (sequence . replicate (n - 2) . forkIO) worker
      >>= flip (flip runMaster sim) st
  pure $ MasterHandle
    { mhThreadId = masterThreadId
    , mhPaused   = pausedRef
    , mhSpeed    = speedRef
    }

runWorker :: (p -> IO p) -> TChan p -> IO () -> IO ()
runWorker f partsChan sig =
  forever $ atomically (readTChan partsChan) >>= f >>= atomically . writeTChan partsChan >> sig

runMaster :: Master g p -> Sim g p -> g -> IO ()
runMaster Master{..} Sim{..} st = flip evalStateT st $ forever $ do
  -- Check if paused
  paused <- liftIO $ readIORef mPaused
  -- Read the current speed
  speed <- if paused then pure pausedSpeed else liftIO (readIORef mSpeed)
  -- Record the current time
  startTime <- liftIO getCurrentTime
  -- Read the current state
  global <- get
  -- Run one simulation step if not paused
  when paused $ do
    -- Split the current state into parts
    let parts = simSplit global
    -- Send the parts out to the workers
    liftIO $ mapM_ (atomically . writeTChan mParts) parts
    -- Count the number of parts
    let n_parts = length parts
    -- Wait for the counter to reach the number of parts
    liftIO $ Sem.wait mCounter n_parts
    -- Merge the outputs from the workers
    -- Run the final step on the merged state
    liftIO (
      simMerge
        <$> replicateM n_parts (atomically $ readTChan mParts)
        <*> pure global
      >>= simFinal) >>= put
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

