
module Simulation where

import Control.Concurrent
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

data SimCommand = Stop | TogglePause | FrameDuration DiffTime deriving (Eq, Ord, Show)

data Master g p =
  Master
  { mState    :: IORef g
  , mCommands :: ReadChan SimCommand
  , mPartsOut :: WriteChan p
  , mPartsIn  :: ReadChan p
  , mSpeed    :: IORef DiffTime
  , mPaused   :: IORef Bool
  , mWorkers  :: [ThreadId]
  }

defaultSpeed :: DiffTime
defaultSpeed = 1

forkSim :: Sim g p -> g -> IO (ThreadId, WriteChan SimCommand)
forkSim sim st = do
  (rc, wc) <- newSplitChan
  fmap (, wc) . forkIO $ do
    n <- getNumCapabilities
    (rcOut, wcOut) <- newSplitChan
    (rcIn, wcIn) <- newSplitChan
    let worker = runWorker (simPart sim) rcOut wcIn
    master <- Master
      <$> newIORef st
      <*> pure rc
      <*> pure wcOut
      <*> pure rcIn
      <*> newIORef defaultSpeed
      <*> newIORef True
      <*> (sequence . replicate (n - 2) . forkIO) worker
    runMaster master

runWorker :: (p -> IO p) -> ReadChan p -> WriteChan p -> IO ()
runWorker f rc wc = todo f rc wc

runMaster :: Master g p -> IO ()
runMaster Master{..} = forever $ todo

