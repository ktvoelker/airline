
module Main where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as TIO
import H.Common
import qualified Network.Wai as WAI
import qualified Network.Wai.Application.Static as Stat
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WAIWS
import qualified Network.WebSockets as WS

webSocketServer :: WS.ServerApp
webSocketServer = WS.acceptRequest >=> handler

handler :: WS.Connection -> IO ()
handler conn = WS.receive conn >>= \case
  (WS.ControlMessage (WS.Close _)) -> WS.send conn $ WS.ControlMessage $ WS.Close mempty
  (WS.ControlMessage _) -> handler conn
  (WS.DataMessage (WS.Text _)) -> handler conn
  (WS.DataMessage (WS.Binary bs)) -> handleMessage (decodeMessage bs) >> handler conn

type Message = ()

decodeMessage :: BSL.ByteString -> Message
decodeMessage _ = ()

handleMessage :: Message -> IO ()
handleMessage _ = TIO.putStrLn "Got a message!"

app :: WAI.Application
app = Stat.staticApp $ Stat.defaultFileServerSettings "./static"

settings :: Warp.Settings
settings = Warp.defaultSettings
  { Warp.settingsIntercept = WAIWS.intercept webSocketServer
  , Warp.settingsPort = 8042
  }

main :: IO ()
main = Warp.runSettings settings app

