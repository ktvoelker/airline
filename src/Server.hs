
module Server
  ( Handler
  , maxClients
  , ClientId()
  , Server()
  , run
  , writeMessage
  ) where

import Control.Concurrent
import Data.IORef
import qualified Data.Map as M
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS

type Handler = Server -> ClientId -> WS.Message -> IO ()

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

data Client = Client
  { cId         :: ClientId
  , cReadThread :: ThreadId
  , cConnection :: WS.Connection
  }

instance Eq Client where
  (==) = (==) `on` cId

instance Ord Client where
  compare = compare `on` cId

instance Show Client where
  showsPrec p = showsPrec p . cId

data Server = Server
  { sNextClientId :: IORef ClientId
  , sClients      :: IORef (M.Map ClientId Client)
  , sHandler      :: Handler
  }

newServer :: Handler -> IO Server
newServer h = do
  nextClientIdRef <- newIORef firstClientId
  clientsRef      <- newIORef M.empty
  return $ Server nextClientIdRef clientsRef h

handleClient :: Server -> WS.PendingConnection -> IO ()
handleClient server@Server{..} p = readIORef sClients >>= \case
  set
    | M.size set > maxClients
      -> WS.rejectRequest p "Too many clients"
    | otherwise
      -> WS.acceptRequest p >>= initForClient server

initForClient :: Server -> WS.Connection -> IO ()
initForClient server@Server{..} conn = do
  cid <- readIORef sNextClientId
  writeIORef sNextClientId $ nextClientId cid
  tid <- forkIO $ forever $ WS.receive conn >>= sHandler server cid
  modifyIORef' sClients $ M.insert cid $ Client cid tid conn

writeMessage :: Server -> ClientId -> WS.Message -> IO ()
writeMessage Server{..} cid msg = do
  (readIORef sClients >>=) $ M.lookup cid >>> \case
    Nothing         -> todo
    Just Client{..} -> WS.send cConnection msg

app :: WAI.Application
app = Stat.staticApp $ Stat.defaultFileServerSettings "./static"

settings :: Server -> Warp.Settings
settings server = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept $ handleClient server
  , Warp.settingsPort      = 8042
  }

run :: Handler -> IO ThreadId
run h = newServer h >>= forkIO . flip Warp.runSettings app . settings

