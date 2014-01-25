
module Main where

import Control.Concurrent.Future
import H.Common

import qualified Message
import Server
import Types ()
import Types.Comm ()
import Types.State ()

handler :: Handler
handler server cid msg = do
  print msg
  let obj = Message.decode msg
  print obj
  maybe (return ()) (writeMessage server cid . Message.encode) obj

main :: IO ()
main = void $ forkPromise (run handler) >>= wait

