
module Server where

import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import H.Common

-- TODO configurable
maxClients :: Int
maxClients = 9

newtype ClientId = ClientId
  { cidInteger :: Integer
  } deriving (Eq, Ord, Show)

firstClientId :: ClientId
firstClientId = ClientId 0

nextClientId :: ClientId -> ClientId
nextClientId = ClientId . (+ 1) . cidInteger

data Client c = Client
  { cId         :: ClientId
  , cReadThread :: ThreadId
  , cConnection :: c
  }

instance Eq (Client c) where
  (==) = (==) `on` cId

instance Ord (Client c) where
  compare = compare `on` cId

instance Show (Client c) where
  showsPrec p = showsPrec p . cId

data Server p c w = Server
  { sMasterThread :: IORef (Maybe (ThreadId))
  , sNextClientId :: IORef ClientId
  , sClients      :: IORef (S.Set (Client c))
  , sHandleClient :: p -> IO ()
  , sWriteMessage :: Client c -> w -> IO ()
  }

data Config p c r w = Config
  { waitForClient :: IO p
  , acceptClient  :: p -> IO c
  , rejectClient  :: p -> IO ()
  , getMessage    :: c -> IO r
  , putMessage    :: c -> w -> IO ()
  , readMessage   :: ClientId -> r -> IO ()
  }

newServer :: Config p c r w -> IO (Server p c w)
newServer Config{..} = do
  masterThreadRef <- newIORef Nothing
  nextClientIdRef <- newIORef firstClientId
  clientsRef      <- newIORef S.empty
  let
  { init conn = do
      cid <- readIORef nextClientIdRef
      writeIORef nextClientIdRef $ nextClientId cid
      tid <- forkIO $ forever $ getMessage conn >>= readMessage cid
      modifyIORef' clientsRef $ S.insert $ Client cid tid conn
  }
  let
  { handler p = readIORef clientsRef >>= \case
      set
        | S.size set > maxClients
          -> rejectClient p
        | otherwise
          -> acceptClient p >>= init
  }
  return
    $ Server masterThreadRef nextClientIdRef clientsRef handler
    $ putMessage . cConnection

startServer :: Server p c w -> IO ()
startServer = todo

stopServer :: Server p c w -> IO ()
stopServer = todo

serverRunning :: Server p c w -> IO Bool
serverRunning = readIORef . sMasterThread >=> return . isJust

handleClient :: Server p c w -> p -> IO ()
handleClient = sHandleClient

writeMessage :: Server p c w -> Client c -> w -> IO ()
writeMessage = sWriteMessage

