
module Simulation where

import Control.Concurrent (ThreadId, forkIO, getNumCapabilities)
import Control.Concurrent.MVar
import qualified Control.Concurrent.MSemN as Sem
import Data.IORef
import Data.Time
import H.Chan
import H.Common

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
  , mSpeed    :: IORef DiffTime
  , mCounter  :: Sem.MSemN Int
  , mWorkers  :: [ThreadId]
  }

defaultSpeed :: DiffTime
defaultSpeed = 1

forkSim :: Sim g p -> g -> IO (ThreadId, (MVar (), IORef DiffTime))
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
      >>= flip runMaster st

runWorker :: (p -> IO p) -> ReadChan p -> WriteChan p -> IO () -> IO ()
runWorker f rc wc sig = forever $ readChan rc >>= f >>= writeChan wc >> sig

runMaster :: Master g p -> g -> IO ()
runMaster Master{..} st = void $ flip execStateT st $ forever $ do
  todo
  -- Wait until un-paused
  -- Read the current speed
  -- Record the current time
  -- Split the current state and send it out to the workers
  -- Record the number of parts
  -- Wait for the counter to reach the number of parts
  -- Merge the outputs from the workers
  -- If too much time has elapsed, log a warning
  -- Otherwise, sleep until enough time has elapsed

