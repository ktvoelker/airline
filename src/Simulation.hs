
module Simulation where

import Control.Concurrent (ThreadId, forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent.MVar
import qualified Control.Concurrent.MSemN as Sem
import Data.IORef
import Data.List (replicate)
import Data.Time
import qualified Data.Text.IO as TIO
import H.Chan
import H.Prelude
import H.IO
import Prelude (subtract)

data Sim g p =
  Sim
  { simPart  :: p -> IO p
  , simSplit :: g -> [p]
  , simMerge :: [p] -> g -> g
  , simFinal :: g -> IO g
  }

data Master g p =
  Master
  { mPartsOut :: WriteChan p
  , mPartsIn  :: ReadChan p
  , mPaused   :: MVar ()
  , mSpeed    :: IORef NominalDiffTime
  , mCounter  :: Sem.MSemN Int
  , mWorkers  :: [ThreadId]
  }

defaultSpeed :: NominalDiffTime
defaultSpeed = 1

forkSim :: Sim g p -> g -> IO (ThreadId, (MVar (), IORef NominalDiffTime))
forkSim sim st = do
  pausedVar <- newEmptyMVar
  speedRef <- newIORef defaultSpeed
  fmap (, (pausedVar, speedRef)) . forkIO $ do
    n <- getNumCapabilities
    (rcOut, wcOut) <- newSplitChan
    (rcIn, wcIn) <- newSplitChan
    sem <- Sem.new 0
    let worker = runWorker (simPart sim) rcOut wcIn (Sem.signal sem 1)
    Master wcOut rcIn pausedVar speedRef sem
      <$> (sequence . replicate (n - 2) . forkIO) worker
      >>= flip (flip runMaster sim) st

runWorker :: (p -> IO p) -> ReadChan p -> WriteChan p -> IO () -> IO ()
runWorker f rc wc sig = forever $ readChan rc >>= f >>= writeChan wc >> sig

runMaster :: Master g p -> Sim g p -> g -> IO ()
runMaster Master{..} Sim{..} st = flip evalStateT st $ forever $ do
  -- Wait until un-paused
  liftIO $ takeMVar mPaused
  -- Read the current speed
  speed <- liftIO $ readIORef mSpeed
  -- Record the current time
  startTime <- liftIO getCurrentTime
  -- Read the current state
  global <- get
  -- Split the current state into parts
  let parts = simSplit global
  -- Send the parts out to the workers
  liftIO $ mapM_ (writeChan mPartsOut) parts
  -- Count the number of parts
  let n_parts = length parts
  -- Wait for the counter to reach the number of parts
  liftIO $ Sem.wait mCounter n_parts
  -- Merge the outputs from the workers
  -- Run the final step on the merged state
  liftIO (
    simMerge
      <$> replicateM n_parts (readChan mPartsIn)
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

