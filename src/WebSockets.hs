
module WebSockets (run) where

import Control.Concurrent (ThreadId, forkIO)
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS

import qualified Server as S

type Handler = S.ClientId -> WS.Message -> IO ()

type WebSocketServer = S.Server WS.PendingConnection WS.Connection WS.Message

type Config = S.Config WS.PendingConnection WS.Connection WS.Message WS.Message

webSocketServer :: WebSocketServer -> WS.PendingConnection -> IO ()
webSocketServer = S.handleClient

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

settings :: WebSocketServer -> Warp.Settings
settings server = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept $ webSocketServer server
  , Warp.settingsPort = 8042
  }

serverConfig :: Handler -> Config
serverConfig h = S.Config
  { S.waitForClient = undefined
  , S.acceptClient  = WS.acceptRequest
  , S.rejectClient  = flip WS.rejectRequest ""
  , S.getMessage    = WS.receive
  , S.putMessage    = WS.send
  , S.readMessage   = h
  }

run :: Handler -> IO ThreadId
run h = S.newServer (serverConfig h) >>= forkIO . flip Warp.runSettings app . settings

