
module Server
  ( Handler
  , maxClients
  , ClientId()
  , Server()
  , run
  , writeMessage
  ) where

import qualified Control.Exception as Exn
import Data.IORef
import qualified Data.Map as M
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS
import System.IO (putStrLn)

type Handler = Server -> ClientId -> WS.DataMessage -> IO ()

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

data Server = Server
  { sNextClientId :: IORef ClientId
  , sClients      :: IORef (M.Map ClientId WS.Connection)
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
    | M.size set >= maxClients
      -> WS.rejectRequest p "Too many clients"
    | otherwise
      -> WS.acceptRequest p >>= initForClient server

initForClient :: Server -> WS.Connection -> IO ()
initForClient server@Server{..} conn = do
  cid <- atomicModifyIORef' sNextClientId $ \cid -> (nextClientId cid, cid)
  putStrLn $ "Opened: " ++ show cid
  atomicModifyIORef' sClients $ (, ()) . M.insert cid conn
  Exn.catch (forever $ WS.receiveDataMessage conn >>= sHandler server cid)
    $ \WS.ConnectionClosed -> putStrLn $ "Closed: " ++ show cid

writeMessage :: Server -> ClientId -> WS.DataMessage -> IO ()
writeMessage Server{..} cid msg = do
  (readIORef sClients >>=) $ M.lookup cid >>> \case
    Nothing         -> error $ "Unknown ClientId: " ++ show cid
    Just conn -> WS.sendDataMessage conn msg

app :: WAI.Application
app = Stat.staticApp $ Stat.defaultFileServerSettings "./static"

settings :: Server -> Warp.Settings
settings server = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept $ handleClient server
  , Warp.settingsPort      = 8042
  }

run :: Handler -> IO ()
run h = newServer h >>= flip Warp.runSettings app . settings

