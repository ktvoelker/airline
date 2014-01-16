
module Server where

import Control.Concurrent
import Data.IORef
import qualified Data.Set as S
import H.Common

data Client c = Client
  { cId         :: Integer
  , cReadThread :: ThreadId
  , cConnection :: c
  }

instance Eq (Client c) where
  (==) = (==) `on` cId

instance Ord (Client c) where
  compare = compare `on` cId

instance Show (Client c) where
  showsPrec p = showsPrec p . cId

data Server m p c w = Server
  { sMasterThread :: IORef (Maybe (ThreadId))
  , sNextClientId :: IORef Integer
  , sClients      :: IORef (S.Set (Client c))
  , sHandleClient :: p -> m ()
  , sWriteMessage :: Client c -> w -> m ()
  }

data Config m p c r w = Config
  { waitForClient :: m p
  , acceptClient  :: p -> m c
  , rejectClient  :: p -> m ()
  , getMessage    :: c -> m r
  , putMessage    :: c -> w -> m ()
  , readMessage   :: Client c -> r -> m ()
  }

newServer :: (MonadIO m) => Config m p c r w -> m (Server m p c w)
newServer = todo

startServer :: (MonadIO m) => Server m p c w -> m ()
startServer = todo

stopServer :: (MonadIO m) => Server m p c w -> m ()
stopServer = todo

serverRunning :: (MonadIO m) => Server m p c w -> m Bool
serverRunning = liftIO . readIORef . sMasterThread >=> return . isJust

handleClient :: (MonadIO m) => Server m p c w -> p -> m ()
handleClient = sHandleClient

writeMessage :: (MonadIO m) => Server m p c w -> Client c -> w -> m ()
writeMessage = sWriteMessage

