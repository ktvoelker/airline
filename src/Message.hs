
module Message where

import qualified Data.Aeson as JSON
import H.Common
import qualified Network.WebSockets as WS

type Message = JSON.Value

encode :: Message -> WS.DataMessage
encode = WS.Text . JSON.encode

decode :: WS.DataMessage -> Maybe Message
decode (WS.Text bs)  = JSON.decode bs
decode (WS.Binary _) = Nothing

