
module Server where

import Control.Concurrent
import Data.IORef
import qualified Data.Map as M
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS

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
  }

newServer :: IO Server
newServer = do
  nextClientIdRef <- newIORef firstClientId
  clientsRef      <- newIORef M.empty
  return $ Server nextClientIdRef clientsRef

handleClient :: Server -> WS.PendingConnection -> IO ()
handleClient server@Server{..} p = readIORef sClients >>= \case
  set
    | M.size set > maxClients
      -> WS.rejectRequest p "Too many clients"
    | otherwise
      -> WS.acceptRequest p >>= initForClient server

initForClient :: Server -> WS.Connection -> IO ()
initForClient Server{..} conn = do
  cid <- readIORef sNextClientId
  writeIORef sNextClientId $ nextClientId cid
  tid <- forkIO $ forever $ WS.receive conn >>= readMessage cid
  modifyIORef' sClients $ M.insert cid $ Client cid tid conn

readMessage :: ClientId -> WS.Message -> IO ()
readMessage = error "This should actually be a callback sent into this module"

writeMessage :: Server -> ClientId -> WS.Message -> IO ()
writeMessage = todo

{-
inputHandler :: IO () -> (BS.ByteString -> IO ()) -> WS.Connection -> IO ()
inputHandler h f conn = WS.receive conn >>= \case
  (WS.ControlMessage (WS.Close _)) ->
    (WS.send conn . WS.ControlMessage . WS.Close $ mempty) >> h
  (WS.ControlMessage _)            -> loop
  (WS.DataMessage (WS.Text _))     -> loop
  (WS.DataMessage (WS.Binary bs))  -> (f . BS.concat . BSL.toChunks $ bs) >> loop
  where
    loop = inputHandler h f conn

outputHandler :: (IO BS.ByteString) -> WS.Connection -> IO ()
outputHandler h conn =
  h >>= WS.send conn . WS.DataMessage . WS.Binary . BSL.fromChunks . (: []) >> loop
  where
    loop = outputHandler h conn
-}

app :: WAI.Application
app = Stat.staticApp $ Stat.defaultFileServerSettings "./static"

settings :: Server -> Warp.Settings
settings server = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept $ handleClient server
  , Warp.settingsPort      = 8042
  }

run :: IO ThreadId
run = newServer >>= forkIO . flip Warp.runSettings app . settings

