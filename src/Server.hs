
module Server (Handler, run) where

import Control.Concurrent
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS

newtype ClientId = ClientId Integer deriving (Eq, Show)

nextClientId :: ClientId -> ClientId
nextClientId (ClientId n) = ClientId $ n + 1

data Client = Client ClientId (Chan BS.ByteString)

instance Eq Client where
  (Client a _) == (Client b _) = a == b

instance Show Client where
  showsPrec p (Client (ClientId n) _) = ("(Client " ++) . showsPrec p n . (")" ++)

data Message =
    ClientConnect Client
  | ClientDisconnect Client
  deriving (Eq, Show)

type Handler = Message -> IO ()

webSocketServer :: IORef ClientId -> Handler -> WS.ServerApp
webSocketServer cidRef h = WS.acceptRequest >=> \conn -> do
  cid <- readIORef cidRef
  writeIORef cidRef $ nextClientId cid
  chan <- newChan
  let client = Client cid chan
  h $ ClientConnect client
  -- TODO stop ignoring the ThreadIds
  void . forkIO $ inputHandler (h $ ClientDisconnect client) (writeChan chan) conn
  void . forkIO $ outputHandler (readChan chan) conn

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

app :: WAI.Application
app = Stat.staticApp $ Stat.defaultFileServerSettings "./static"

settings :: IORef ClientId -> Handler -> Warp.Settings
settings cidRef h = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept $ webSocketServer cidRef h
  , Warp.settingsPort = 8042
  }

run :: Handler -> IO ThreadId
run h = do
  cidRef <- newIORef $ ClientId 0
  forkIO $ Warp.runSettings (settings cidRef h) app

