
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
import Prelude (subtract)

data Sim g p c r =
  Sim
  { simPart  :: p -> IO p
  , simSplit :: g -> [p]
  , simMerge :: [p] -> g -> g
  , simFinal :: g -> IO g
  , simUser  :: c -> StateT g IO r
  }

data Master g p c r =
  Master
  { mParts     :: TChan p
  , mPaused    :: IORef Bool
  , mSpeed     :: IORef NominalDiffTime
  , mCounter   :: Sem.MSemN Int
  , mCommands  :: TChan c
  , mResponses :: TChan r
  , mWorkers   :: [ThreadId]
  }

data MasterHandle g p c r =
  MasterHandle
  { mhThreadId  :: ThreadId
  , mhPaused    :: IORef Bool
  , mhSpeed     :: IORef NominalDiffTime
  , mhCommands  :: TChan c
  , mhResponses :: TChan r
  }

pausedSpeed :: NominalDiffTime
pausedSpeed = 0.1

userInputCutOff :: NominalDiffTime
userInputCutOff = 0.05

forkSim :: NominalDiffTime -> Sim g p c r -> g -> IO (MasterHandle g p c r)
forkSim startSpeed sim st = do
  pausedRef <- newIORef True
  speedRef <- newIORef startSpeed
  cmdChan <- newTChanIO
  respChan <- newTChanIO
  masterThreadId <- forkIO $ do
    n <- getNumCapabilities
    partsChan <- newTChanIO
    sem <- Sem.new 0
    let worker = runWorker (simPart sim) partsChan (Sem.signal sem 1)
    Master partsChan pausedRef speedRef sem cmdChan respChan
      <$> (sequence . replicate (n - 2) . forkIO) worker
      >>= flip (flip runMaster sim) st
  pure $ MasterHandle
    { mhThreadId  = masterThreadId
    , mhPaused    = pausedRef
    , mhSpeed     = speedRef
    , mhCommands  = cmdChan
    , mhResponses = respChan
    }

runWorker :: (p -> IO p) -> TChan p -> IO () -> IO ()
runWorker f partsChan sig =
  forever $ atomically (readTChan partsChan) >>= f >>= atomically . writeTChan partsChan >> sig

runMaster :: Master g p c r -> Sim g p c r -> g -> IO ()
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
  -- Handle user input
  handleUserInput startTime speed mCommands mResponses simUser
  -- Calculate the remaining time for this cycle
  remaining <- (`subtract` speed) . (`diffUTCTime` startTime) <$> liftIO getCurrentTime
  -- If too much time has elapsed, log a warning
  -- Otherwise, sleep until enough time has elapsed
  if remaining < 0
    then liftIO $ TIO.putStrLn $ "Cycle took too long! " <> show remaining
    else liftIO $ threadDelay' remaining

handleUserInput :: UTCTime -> NominalDiffTime -> TChan c -> TChan r -> (c -> StateT g IO r) -> StateT g IO ()
handleUserInput startTime speed readCmd writeResp handler = do
  remaining <- (`subtract` speed) . (`diffUTCTime` startTime) <$> liftIO getCurrentTime
  case remaining > userInputCutOff of
    False -> liftIO $ do
      moreCommands <- atomically $ isEmptyTChan readCmd
      when moreCommands $ TIO.putStrLn "Unprocessed user commands remain at end of cycle!"
    True -> do
      mCmd <- liftIO $ atomically $ tryReadTChan readCmd
      whenJust mCmd $ \cmd -> do
        handler cmd >>= liftIO . atomically . writeTChan writeResp
        handleUserInput startTime speed readCmd writeResp handler

threadDelay' :: NominalDiffTime -> IO ()
threadDelay' = threadDelay . round . (* microsecondsPerSecond) . toRational

microsecondsPerSecond :: (Num a) => a 
microsecondsPerSecond = 1000000

